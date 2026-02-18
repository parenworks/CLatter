(in-package #:clatter.net.irc)

;;; Real IRC client using usocket + cl+ssl

(defclass irc-connection ()
  ((network-config :initarg :network-config :accessor irc-network-config)
   (socket         :initform nil :accessor irc-socket)
   (stream         :initform nil :accessor irc-stream)
   (state          :initform :disconnected :accessor irc-state) ; :disconnected :connecting :registering :connected
   (nick           :initform nil :accessor irc-nick)
   (thread         :initform nil :accessor irc-thread)
   (app            :initarg :app :accessor irc-app)
   (network-id     :initarg :network-id :accessor irc-network-id)
   (write-lock     :initform (bordeaux-threads:make-lock "irc-write") :accessor irc-write-lock)
   ;; SASL state
   (sasl-state     :initform nil :accessor irc-sasl-state)  ; nil :requested :authenticating :done
   (cap-negotiating :initform nil :accessor irc-cap-negotiating)
   ;; IRCv3 capabilities - track what's enabled
   (cap-enabled    :initform nil :accessor irc-cap-enabled)  ; list of enabled caps
   ;; IRCv3 batch tracking - hash of batch-id -> (type target messages)
   (active-batches :initform (make-hash-table :test 'equal) :accessor irc-active-batches)
   ;; IRCv3 labeled-response tracking - hash of label -> callback function
   (pending-labels :initform (make-hash-table :test 'equal) :accessor irc-pending-labels)
   (label-counter  :initform 0 :accessor irc-label-counter)
   ;; Reconnection state
   (reconnect-enabled :initform t :accessor irc-reconnect-enabled)
   (reconnect-attempts :initform 0 :accessor irc-reconnect-attempts)
   (max-reconnect-delay :initform 300 :accessor irc-max-reconnect-delay)  ; 5 minutes max
   ;; Health monitoring
   (last-activity  :initform (get-universal-time) :accessor irc-last-activity)
   (ping-sent-time :initform nil :accessor irc-ping-sent-time)))

;; IRCv3 capabilities - use clatter.core.constants:+wanted-capabilities+

(defun make-irc-connection (app network-id network-config)
  (make-instance 'irc-connection
                 :app app
                 :network-id network-id
                 :network-config network-config))

(defun dispatch-event (conn event)
  "Dispatch an event to be handled on the main thread.
   Events from IRC are received on background threads, so we use
   croatoan:submit to run the handler on the main event loop."
  (let ((app (irc-app conn)))
    (clatter.ui.tui:ui-submit
      (clatter.core.events:handle-event event app))))

(defun irc-send (conn line)
  "Send a line to the IRC server (thread-safe)."
  (bordeaux-threads:with-lock-held ((irc-write-lock conn))
    (let ((stream (irc-stream conn)))
      (when stream
        (write-string line stream)
        (write-char #\Return stream)
        (write-char #\Linefeed stream)
        (force-output stream)))))

(defun irc-connect (conn)
  "Establish connection to IRC server."
  (let* ((cfg (irc-network-config conn))
         (server (clatter.core.config:network-config-server cfg))
         (port (clatter.core.config:network-config-port cfg))
         (use-tls (clatter.core.config:network-config-tls cfg))
         (client-cert (clatter.core.config:network-config-client-cert cfg)))
    (setf (irc-state conn) :connecting)
    (handler-case
        (let ((sock (usocket:socket-connect server port :element-type '(unsigned-byte 8))))
          (setf (irc-socket conn) sock)
          (let ((raw-stream (usocket:socket-stream sock)))
            (setf (irc-stream conn)
                  (if use-tls
                      (if (and client-cert (probe-file client-cert))
                          ;; TLS with client certificate for SASL EXTERNAL
                          (cl+ssl:make-ssl-client-stream
                           raw-stream
                           :hostname server
                           :certificate client-cert
                           :key client-cert
                           :external-format :utf-8)
                          ;; TLS without client certificate
                          (cl+ssl:make-ssl-client-stream
                           raw-stream
                           :hostname server
                           :external-format :utf-8))
                      (flexi-streams:make-flexi-stream
                       raw-stream
                       :external-format :utf-8))))
          (setf (irc-state conn) :registering)
          t)
      (error (e)
        (setf (irc-state conn) :disconnected)
        (irc-log-error conn "Connection failed: ~a" e)
        nil))))

(defun irc-disconnect (conn &optional message)
  "Disconnect from IRC server."
  (when (irc-stream conn)
    (ignore-errors
      (when message
        (irc-send conn (clatter.core.protocol:irc-quit message))))
    (ignore-errors (close (irc-stream conn))))
  (when (irc-socket conn)
    (ignore-errors (usocket:socket-close (irc-socket conn))))
  (setf (irc-stream conn) nil
        (irc-socket conn) nil
        (irc-state conn) :disconnected))

(defun base64-encode (string)
  "Encode a string to base64."
  (cl-base64:string-to-base64-string string))

(defun irc-register (conn)
  "Send registration commands, always negotiating IRCv3 capabilities."
  (let* ((cfg (irc-network-config conn))
         (nick (clatter.core.config:network-config-nick cfg)))
    (setf (irc-nick conn) nick)
    ;; Always start CAP negotiation for IRCv3 capabilities
    (setf (irc-cap-negotiating conn) t)
    (irc-send conn "CAP LS 302")))

(defun irc-send-registration (conn)
  "Send NICK and USER commands."
  (let* ((cfg (irc-network-config conn))
         (nick (clatter.core.config:network-config-nick cfg))
         (username (or (clatter.core.config:network-config-username cfg) nick))
         (realname (clatter.core.config:network-config-realname cfg))
         (password (clatter.core.config:get-server-password cfg)))
    (when password
      (irc-send conn (clatter.core.protocol:irc-pass password)))
    (irc-send conn (clatter.core.protocol:irc-nick nick))
    (irc-send conn (clatter.core.protocol:irc-user username realname))))

(defun irc-sasl-authenticate (conn)
  "Send SASL PLAIN authentication."
  (let* ((cfg (irc-network-config conn))
         (nick (clatter.core.config:network-config-nick cfg))
         (password (clatter.core.config:get-network-password cfg))
         ;; SASL PLAIN format: \0username\0password (base64 encoded)
         (auth-string (format nil "~c~a~c~a" #\Nul nick #\Nul password))
         (encoded (base64-encode auth-string)))
    (setf (irc-sasl-state conn) :authenticating)
    (irc-send conn (format nil "AUTHENTICATE ~a" encoded))))

(defun parse-cap-list (caps-string)
  "Parse a space-separated capability list, handling values like 'cap=value'."
  (when caps-string
    (mapcar (lambda (cap)
              (let ((eq-pos (position #\= cap)))
                (if eq-pos
                    (subseq cap 0 eq-pos)  ; strip value part
                    cap)))
            (uiop:split-string caps-string :separator " "))))

(defun find-matching-caps (available wanted)
  "Find capabilities from WANTED that are in AVAILABLE."
  (remove-if-not (lambda (w)
                   (member w available :test #'string-equal))
                 wanted))

(defun irc-handle-cap (conn params)
  "Handle CAP response for IRCv3 capability negotiation."
  (let* ((subcommand (second params))
         (caps-raw (or (third params) ""))
         ;; Handle multi-line CAP LS (marked with *)
         (is-multiline (string= (second params) "*"))
         (caps-string (if is-multiline (fourth params) caps-raw)))
    (cond
      ;; CAP LS response - request capabilities we want
      ((or (string-equal subcommand "LS")
           (and is-multiline (string-equal (third params) "LS")))
       (let* ((available (parse-cap-list caps-string))
              (cfg (irc-network-config conn))
              (sasl-type (clatter.core.config:network-config-sasl cfg))
              (client-cert (clatter.core.config:network-config-client-cert cfg))
              ;; Want SASL EXTERNAL if configured and have cert
              (want-sasl-external (and (eq sasl-type :external)
                                       client-cert
                                       (probe-file client-cert)))
              ;; Want SASL PLAIN if configured and have password
              (want-sasl-plain (and (eq sasl-type :plain)
                                    (clatter.core.config:get-network-password cfg)))
              (caps-to-request (find-matching-caps available clatter.core.constants:+wanted-capabilities+)))
         ;; Add sasl if we want it and it's available
         (when (and (or want-sasl-external want-sasl-plain)
                    (member "sasl" available :test #'string-equal))
           (push "sasl" caps-to-request)
           (setf (irc-sasl-state conn) :requested))
         (if caps-to-request
             (progn
               (irc-log-system conn "Requesting capabilities: ~{~a~^, ~}" caps-to-request)
               (irc-send conn (format nil "CAP REQ :~{~a~^ ~}" caps-to-request)))
             (progn
               (irc-log-system conn "No IRCv3 capabilities to request")
               (irc-send conn "CAP END")
               (irc-send-registration conn)))))
      
      ;; CAP ACK - capabilities accepted
      ((string-equal subcommand "ACK")
       (let* ((acked (parse-cap-list caps-string))
              (cfg (irc-network-config conn))
              (sasl-type (clatter.core.config:network-config-sasl cfg)))
         (setf (irc-cap-enabled conn) (append (irc-cap-enabled conn) acked))
         (irc-log-system conn "Enabled capabilities: ~{~a~^, ~}" acked)
         ;; If SASL was acked, start authentication
         (if (member "sasl" acked :test #'string-equal)
             (cond
               ;; SASL EXTERNAL - certificate-based auth
               ;; Don't send registration yet - wait for AUTHENTICATE + response
               ((eq sasl-type :external)
                (irc-log-system conn "Starting SASL EXTERNAL authentication")
                (setf (irc-sasl-state conn) :authenticating)
                (irc-send conn "AUTHENTICATE EXTERNAL"))
               ;; SASL PLAIN - password-based auth
               ;; Don't send registration yet - wait for AUTHENTICATE + response
               ((eq sasl-type :plain)
                (setf (irc-sasl-state conn) :authenticating)
                (irc-send conn "AUTHENTICATE PLAIN"))
               ;; Fallback
               (t
                (irc-send conn "CAP END")
                (irc-send-registration conn)))
             (progn
               (irc-send conn "CAP END")
               (irc-send-registration conn)))))
      
      ;; CAP NAK - capabilities rejected
      ((string-equal subcommand "NAK")
       (irc-log-system conn "Capabilities rejected: ~a" caps-string)
       (irc-send conn "CAP END")
       (irc-send-registration conn)))))

(defun irc-log-error (conn format-string &rest args)
  "Log error to server buffer for this connection's network."
  (let ((app (irc-app conn))
        (text (apply #'format nil format-string args)))
    (clatter.ui.tui:ui-submit
      (let ((buf (irc-find-server-buffer conn)))
        (when buf
          (clatter.core.dispatch:deliver-message
           app buf
           (clatter.core.model:make-message :level :error :nick "*error*" :text text)))))))

(defun irc-log-system (conn format-string &rest args)
  "Log system message to server buffer for this connection's network."
  (let ((app (irc-app conn))
        (text (apply #'format nil format-string args)))
    (clatter.ui.tui:ui-submit
      (let ((buf (irc-find-server-buffer conn)))
        (when buf
          (clatter.core.dispatch:deliver-message
           app buf
           (clatter.core.model:make-message :level :system :nick "*" :text text)))))))

(defun irc-handle-message (conn msg)
  "Handle a parsed IRC message."
  (let ((command (clatter.core.protocol:irc-message-command msg))
        (params (clatter.core.protocol:irc-message-params msg))
        (prefix (clatter.core.protocol:irc-message-prefix msg))
        (tags (clatter.core.protocol:irc-message-tags msg)))
    ;; Check for labeled-response
    (let* ((parsed-tags (clatter.core.protocol:parse-irc-tags tags))
           (label (cdr (assoc "label" parsed-tags :test #'string=))))
      (when label
        (irc-handle-labeled-response conn label msg)))
    (cond
      ;; PING -> PONG
      ((string= command "PING")
       (setf (irc-last-activity conn) (get-universal-time))
       (irc-send conn (clatter.core.protocol:irc-pong (or (first params) ""))))
      
      ;; PONG - response to our health check ping
      ((string= command "PONG")
       (setf (irc-last-activity conn) (get-universal-time))
       (setf (irc-ping-sent-time conn) nil))
      
      ;; CAP response - handle SASL negotiation
      ((string= command "CAP")
       (irc-handle-cap conn params))
      
      ;; AUTHENTICATE response
      ((string= command "AUTHENTICATE")
       (when (string= (first params) "+")
         (let* ((cfg (irc-network-config conn))
                (sasl-type (clatter.core.config:network-config-sasl cfg)))
           (if (eq sasl-type :external)
               ;; SASL EXTERNAL - just send "+" (empty response, cert already presented)
               (irc-send conn "AUTHENTICATE +")
               ;; SASL PLAIN - send credentials
               (irc-sasl-authenticate conn)))))
      
      ;; 903 RPL_SASLSUCCESS
      ((string= command "903")
       (setf (irc-sasl-state conn) :done)
       (irc-log-system conn "SASL authentication successful")
       (irc-send conn "CAP END")
       (irc-send-registration conn))
      
      ;; 904, 905 SASL failures
      ((or (string= command "904") (string= command "905"))
       (irc-log-error conn "SASL authentication failed: ~a" (second params))
       (irc-send conn "CAP END")
       (irc-send-registration conn))
      
      ;; 001 RPL_WELCOME - registration complete
      ((string= command "001")
       (setf (irc-state conn) :connected)
       (irc-log-system conn "Connected to ~a as ~a"
                       (clatter.core.config:network-config-server (irc-network-config conn))
                       (irc-nick conn))
       ;; NickServ identify if configured AND we didn't use SASL
       (let ((ns-pw (clatter.core.config:get-network-password (irc-network-config conn))))
         (when (and ns-pw (not (eq (irc-sasl-state conn) :done)))
           (irc-send conn (clatter.core.protocol:irc-privmsg "NickServ"
                                                             (format nil "IDENTIFY ~a" ns-pw)))))
       ;; If we used SASL and got a different nick (collision), try to REGAIN
       (let* ((cfg (irc-network-config conn))
              (wanted-nick (clatter.core.config:network-config-nick cfg)))
         (when (and (eq (irc-sasl-state conn) :done)
                    (not (string-equal (irc-nick conn) wanted-nick)))
           (irc-send conn (clatter.core.protocol:irc-privmsg "NickServ"
                                                             (format nil "REGAIN ~a" wanted-nick)))))
       ;; Autojoin channels
       (let ((channels (clatter.core.config:network-config-autojoin (irc-network-config conn))))
         (dolist (ch channels)
           (irc-send conn (clatter.core.protocol:irc-join ch)))))
      
      ;; 433 ERR_NICKNAMEINUSE
      ((string= command "433")
       (let ((new-nick (concatenate 'string (irc-nick conn) "_")))
         (setf (irc-nick conn) new-nick)
         (irc-send conn (clatter.core.protocol:irc-nick new-nick))
         (irc-log-system conn "Nick in use, trying ~a" new-nick)))
      
      ;; PRIVMSG
      ((string= command "PRIVMSG")
       (let* ((target (first params))
              (raw-text (second params))
              (text (clatter.core.protocol:strip-irc-formatting raw-text))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (sender-nick (clatter.core.protocol:prefix-nick parsed-prefix))
              (server-time (clatter.core.protocol:get-server-time tags))
              (parsed-tags (clatter.core.protocol:parse-irc-tags tags))
              (batch-id (cdr (assoc "batch" parsed-tags :test #'string=))))
         ;; Check for CTCP (starts and ends with \x01)
         (cond
           ((and (> (length raw-text) 1)
                 (char= (char raw-text 0) (code-char 1))
                 (char= (char raw-text (1- (length raw-text))) (code-char 1)))
            (irc-handle-ctcp conn sender-nick target raw-text))
           ;; If part of a batch, accumulate instead of delivering
           (batch-id
            (irc-accumulate-batch-message conn batch-id sender-nick text server-time))
           ;; Normal message - dispatch as CLOS event
           (t
            (unless (clatter.core.model:ignored-p (irc-app conn) sender-nick)
              (dispatch-event conn
                (make-instance 'clatter.core.events:privmsg-event
                               :connection conn
                               :sender sender-nick
                               :target target
                               :text text
                               :server-time server-time)))))))
      
      ;; NOTICE
      ((string= command "NOTICE")
       (let* ((target (first params))
              (raw-text (second params))
              (text (clatter.core.protocol:strip-irc-formatting raw-text))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (sender-nick (or (clatter.core.protocol:prefix-nick parsed-prefix) "*")))
         ;; Check for CTCP reply in NOTICE (don't respond, just display)
         (if (and (> (length raw-text) 1)
                  (char= (char raw-text 0) (code-char 1))
                  (char= (char raw-text (1- (length raw-text))) (code-char 1)))
             ;; CTCP reply - just log it, don't respond (prevents loops)
             (let* ((ctcp-content (subseq raw-text 1 (1- (length raw-text))))
                    (space-pos (position #\Space ctcp-content))
                    (ctcp-cmd (if space-pos (subseq ctcp-content 0 space-pos) ctcp-content))
                    (ctcp-args (if space-pos (subseq ctcp-content (1+ space-pos)) "")))
               (irc-log-system conn "CTCP ~a reply from ~a: ~a" ctcp-cmd sender-nick ctcp-args))
             ;; Normal NOTICE - dispatch as CLOS event
             (dispatch-event conn
               (make-instance 'clatter.core.events:notice-event
                              :connection conn
                              :sender sender-nick
                              :target target
                              :text text)))))
      
      ;; JOIN - with extended-join: channel account realname
      ((string= command "JOIN")
       (let* ((channel (first params))
              (account (second params))  ; extended-join: account or "*" if not logged in
              (realname (third params))  ; extended-join: realname
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (nick (clatter.core.protocol:prefix-nick parsed-prefix)))
         (if (string= nick (irc-nick conn))
             ;; We joined - create buffer
             (irc-create-channel-buffer conn channel)
             ;; Someone else joined - dispatch as CLOS event
             (dispatch-event conn
               (make-instance 'clatter.core.events:join-event
                              :connection conn
                              :channel channel
                              :nick nick
                              :account account
                              :realname realname)))))
      
      ;; PART
      ((string= command "PART")
       (let* ((channel (first params))
              (message (second params))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (nick (clatter.core.protocol:prefix-nick parsed-prefix)))
         (dispatch-event conn
           (make-instance 'clatter.core.events:part-event
                          :connection conn
                          :channel channel
                          :nick nick
                          :message message))))
      
      ;; QUIT
      ((string= command "QUIT")
       (let* ((message (first params))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (nick (clatter.core.protocol:prefix-nick parsed-prefix)))
         (dispatch-event conn
           (make-instance 'clatter.core.events:quit-event
                          :connection conn
                          :nick nick
                          :message message))))
      
      ;; NICK - nick change (ours or someone else's)
      ((string= command "NICK")
       (let* ((new-nick (first params))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (old-nick (clatter.core.protocol:prefix-nick parsed-prefix)))
         (if (string-equal old-nick (irc-nick conn))
             ;; Our nick changed (e.g., from REGAIN)
             (progn
               (setf (irc-nick conn) new-nick)
               (irc-log-system conn "Nick changed to ~a" new-nick))
             ;; Someone else changed nick - dispatch as CLOS event
             (dispatch-event conn
               (make-instance 'clatter.core.events:nick-event
                              :connection conn
                              :old-nick old-nick
                              :new-nick new-nick)))))
      
      ;; AWAY - IRCv3 away-notify capability
      ((string= command "AWAY")
       (let* ((parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (nick (clatter.core.protocol:prefix-nick parsed-prefix))
              (away-msg (first params)))
         (dispatch-event conn
           (make-instance 'clatter.core.events:away-event
                          :connection conn
                          :nick nick
                          :message away-msg))))
      
      ;; MODE - channel or user mode change
      ((string= command "MODE")
       (let* ((target (first params))
              (modes (rest params))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (setter (or (clatter.core.protocol:prefix-nick parsed-prefix) target)))
         ;; Dispatch as CLOS event
         (dispatch-event conn
           (make-instance 'clatter.core.events:mode-event
                          :connection conn
                          :target target
                          :setter setter
                          :modes modes))))
      
      ;; 324 RPL_CHANNELMODEIS - channel modes response
      ((string= command "324")
       (let ((channel (second params))
             (modes (format nil "~{~a~^ ~}" (cddr params))))
         (irc-log-system conn "Channel ~a modes: ~a" channel modes)
         (irc-set-channel-modes conn channel modes)))
      
      ;; 353 RPL_NAMREPLY - channel member list
      ((string= command "353")
       ;; params: nick = | @ channel :nick1 nick2 @nick3 +nick4 ...
       (let* ((channel (third params))
              (names-str (fourth params)))
         (when (and channel names-str)
           (irc-add-members conn channel names-str))))
      
      ;; 366 RPL_ENDOFNAMES - end of names list (ignore)
      ((string= command "366")
       nil)
      
      ;; 730 RPL_MONONLINE - monitored user is online
      ((string= command "730")
       (let ((nicks (second params)))
         (when nicks
           (irc-log-system conn "Online: ~a" nicks))))
      
      ;; 731 RPL_MONOFFLINE - monitored user is offline
      ((string= command "731")
       (let ((nicks (second params)))
         (when nicks
           (irc-log-system conn "Offline: ~a" nicks))))
      
      ;; 732 RPL_MONLIST - list of monitored nicks
      ((string= command "732")
       (let ((nicks (second params)))
         (when nicks
           (irc-log-system conn "Monitoring: ~a" nicks))))
      
      ;; 733 RPL_ENDOFMONLIST - end of monitor list
      ((string= command "733")
       nil)
      
      ;; 734 ERR_MONLISTFULL - monitor list is full
      ((string= command "734")
       (irc-log-error conn "Monitor list full (limit: ~a)" (second params)))
      
      ;; TAGMSG - handle typing indicators and other client tags
      ((string= command "TAGMSG")
       (let* ((parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (nick (clatter.core.protocol:prefix-nick parsed-prefix))
              (target (first params))
              (parsed-tags (clatter.core.protocol:parse-irc-tags tags))
              (typing-state (cdr (assoc "+typing" parsed-tags :test #'string=))))
         (when (and typing-state (not (string-equal nick (irc-nick conn))))
           ;; Dispatch as CLOS event (skip our own typing)
           (dispatch-event conn
             (make-instance 'clatter.core.events:typing-event
                            :connection conn
                            :nick nick
                            :target target
                            :state typing-state)))))
      
      ;; BATCH - start or end a batch of related messages
      ((string= command "BATCH")
       (irc-handle-batch conn tags params))
      
      ;; Other numerics - log to server buffer
      ((every #'digit-char-p command)
       (let ((text (format nil "[~a] ~{~a~^ ~}" command (cdr params))))
         (irc-log-system conn "~a" text)))
      
      ;; Default - log unknown
      (t
       (irc-log-system conn "~a ~{~a~^ ~}" command params)))))

(defun irc-handle-ctcp (conn sender-nick target raw-text)
  "Handle incoming CTCP request and send appropriate reply."
  ;; Don't respond to CTCP from ourselves (prevents loops)
  (when (string-equal sender-nick (irc-nick conn))
    (return-from irc-handle-ctcp nil))
  ;; Strip the \x01 delimiters
  (let* ((ctcp-content (subseq raw-text 1 (1- (length raw-text))))
         (space-pos (position #\Space ctcp-content))
         (ctcp-cmd (string-upcase (if space-pos
                                      (subseq ctcp-content 0 space-pos)
                                      ctcp-content)))
         (ctcp-args (if space-pos
                        (subseq ctcp-content (1+ space-pos))
                        "")))
    (cond
      ;; ACTION - dispatch as CLOS event
      ((string= ctcp-cmd "ACTION")
       (dispatch-event conn
         (make-instance 'clatter.core.events:action-event
                        :connection conn
                        :sender sender-nick
                        :target target
                        :text ctcp-args)))
      ;; VERSION - reply with client info
      ((string= ctcp-cmd "VERSION")
       (irc-send conn (clatter.core.protocol:irc-ctcp-reply
                       sender-nick "VERSION" "CLatter 0.0.1 - Common Lisp IRC Client"))
       (irc-log-system conn "CTCP VERSION from ~a" sender-nick))
      ;; PING - echo back the argument
      ((string= ctcp-cmd "PING")
       (irc-send conn (clatter.core.protocol:irc-ctcp-reply sender-nick "PING" ctcp-args))
       (irc-log-system conn "CTCP PING from ~a" sender-nick))
      ;; TIME - reply with current time
      ((string= ctcp-cmd "TIME")
       (let ((time-str (multiple-value-bind (sec min hour day month year)
                           (get-decoded-time)
                         (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                                 year month day hour min sec))))
         (irc-send conn (clatter.core.protocol:irc-ctcp-reply sender-nick "TIME" time-str))
         (irc-log-system conn "CTCP TIME from ~a" sender-nick)))
      ;; DCC - handle DCC offers
      ((string= ctcp-cmd "DCC")
       (let ((manager clatter.net.dcc:*dcc-manager*))
         (when manager
           ;; Parse DCC type and args: "CHAT chat ip port" or "SEND file ip port size"
           (let* ((space-pos2 (position #\Space ctcp-args))
                  (dcc-type (if space-pos2
                                (subseq ctcp-args 0 space-pos2)
                                ctcp-args))
                  (dcc-rest (if space-pos2
                                (subseq ctcp-args (1+ space-pos2))
                                "")))
             (clatter.net.dcc:dcc-handle-offer manager sender-nick dcc-type dcc-rest)))))
      ;; Unknown CTCP - just log it
      (t
       (irc-log-system conn "CTCP ~a from ~a: ~a" ctcp-cmd sender-nick ctcp-args)))))

(defun irc-find-server-buffer (conn)
  "Find the server buffer for this connection's network."
  (let* ((app (irc-app conn))
         (network-name (clatter.core.config:network-config-name (irc-network-config conn)))
         (buffers (clatter.core.model:app-buffers app)))
    (loop for i from 0 below (length buffers)
          for buf = (aref buffers i)
          when (and buf
                    (eq (clatter.core.model:buffer-kind buf) :server)
                    (string-equal (clatter.core.model:buffer-network buf) network-name))
            return buf)))

(defun irc-set-channel-modes (conn channel modes)
  "Set the channel modes string for a channel buffer."
  (let* ((app (irc-app conn))
         (buf (irc-find-buffer conn channel)))
    (when buf
      (clatter.ui.tui:ui-submit
        (setf (clatter.core.model:buffer-channel-modes buf) modes)
        (clatter.core.model:mark-dirty app :status)))))

(defun irc-send-typing (conn target state)
  "Send typing indicator if the capability is enabled.
   STATE should be :active, :paused, or :done."
  (when (or (member "typing" (irc-cap-enabled conn) :test #'string-equal)
            (member "draft/typing" (irc-cap-enabled conn) :test #'string-equal))
    (irc-send conn (clatter.core.protocol:irc-typing target state))))

(defun irc-accumulate-batch-message (conn batch-id sender-nick text server-time)
  "Accumulate a message into an active batch."
  (let ((batch (gethash batch-id (irc-active-batches conn))))
    (when batch
      (let ((msg (clatter.core.model:make-message 
                  :level :chat :nick sender-nick :text text 
                  :ts (or server-time (get-universal-time)))))
        (push msg (getf batch :messages))))))

(defun irc-handle-batch (conn tags params)
  "Handle BATCH command - start or end a batch of messages.
   +ref starts a batch, -ref ends it."
  (let* ((ref (first params))
         (starting (and (> (length ref) 0) (char= (char ref 0) #\+)))
         (batch-id (subseq ref 1)))
    (if starting
        ;; Start new batch
        (let ((batch-type (second params))
              (batch-target (third params)))
          (setf (gethash batch-id (irc-active-batches conn))
                (list :type batch-type :target batch-target :messages nil)))
        ;; End batch - process accumulated messages
        (let ((batch (gethash batch-id (irc-active-batches conn))))
          (when batch
            (let ((batch-type (getf batch :type))
                  (messages (nreverse (getf batch :messages))))
              ;; Handle chathistory batches
              (when (string-equal batch-type "chathistory")
                (irc-deliver-chathistory conn batch messages))
              (remhash batch-id (irc-active-batches conn))))))))

(defun irc-deliver-chathistory (conn batch messages)
  "Deliver chathistory batch messages to the appropriate buffer."
  (let* ((app (irc-app conn))
         (target (getf batch :target)))
    (clatter.ui.tui:ui-submit
      (let ((buf (irc-find-or-create-buffer conn target)))
        (when buf
          (dolist (msg messages)
            (clatter.core.dispatch:deliver-message app buf msg :no-notify t)))))))

(defun irc-request-chathistory (conn target &key (limit 50) before after)
  "Request chat history for TARGET from server.
   LIMIT is max messages, BEFORE/AFTER are timestamps or msgids."
  (when (or (member "chathistory" (irc-cap-enabled conn) :test #'string-equal)
            (member "draft/chathistory" (irc-cap-enabled conn) :test #'string-equal))
    (let ((cmd (cond
                 (before (format nil "CHATHISTORY BEFORE ~a timestamp=~a ~d" target before limit))
                 (after (format nil "CHATHISTORY AFTER ~a timestamp=~a ~d" target after limit))
                 (t (format nil "CHATHISTORY LATEST ~a * ~d" target limit)))))
      (irc-send conn cmd))))

(defun irc-generate-label (conn)
  "Generate a unique label for labeled-response."
  (format nil "clatter~d" (incf (irc-label-counter conn))))

(defun irc-send-labeled (conn command &optional callback)
  "Send a command with a label tag for labeled-response.
   CALLBACK will be called with the response when received.
   Returns the label used."
  (if (member "labeled-response" (irc-cap-enabled conn) :test #'string-equal)
      (let ((label (irc-generate-label conn)))
        (when callback
          (setf (gethash label (irc-pending-labels conn)) callback))
        (irc-send conn (format nil "@label=~a ~a" label command))
        label)
      ;; Fallback: just send without label
      (progn
        (irc-send conn command)
        nil)))

(defun irc-handle-labeled-response (conn label response-data)
  "Handle a labeled response by calling the registered callback."
  (let ((callback (gethash label (irc-pending-labels conn))))
    (when callback
      (funcall callback response-data)
      (remhash label (irc-pending-labels conn)))))

(defun irc-add-members (conn channel names-str)
  "Parse NAMES reply and add members to channel buffer.
   Also detect our own modes from the prefix."
  (let* ((app (irc-app conn))
         (my-nick (irc-nick conn))
         (buf (irc-find-buffer conn channel))
         (found-my-mode nil))
    (when buf
      ;; Parse outside submit, collect data
      (dolist (name (uiop:split-string names-str :separator " "))
        (when (> (length name) 0)
          (let* ((prefix-chars "@+%~&")
                 (first-char (char name 0))
                 (has-prefix (position first-char prefix-chars))
                 (nick (if has-prefix (subseq name 1) name)))
            (when (> (length nick) 0)
              ;; Check if this is us and capture our mode
              (when (and has-prefix (string-equal nick my-nick))
                (setf found-my-mode (string first-char)))))))
      ;; Now submit UI updates
      (clatter.ui.tui:ui-submit
        ;; Re-parse and add members
        (dolist (name (uiop:split-string names-str :separator " "))
          (when (> (length name) 0)
            (let* ((prefix-chars "@+%~&")
                   (first-char (char name 0))
                   (has-prefix (position first-char prefix-chars))
                   (nick (if has-prefix (subseq name 1) name)))
              (when (> (length nick) 0)
                (setf (gethash nick (clatter.core.model:buffer-members buf)) t)))))
        ;; Set our mode if found
        (when found-my-mode
          (unless (search found-my-mode (clatter.core.model:buffer-my-modes buf))
            (setf (clatter.core.model:buffer-my-modes buf)
                  (concatenate 'string (clatter.core.model:buffer-my-modes buf) found-my-mode))))
        (clatter.core.model:mark-dirty app :status)))))

(defvar *buffer-creation-lock* (bordeaux-threads:make-lock "buffer-creation"))

(defun irc-find-buffer (conn target)
  "Find buffer for target (channel or nick) on this connection's network."
  (let* ((app (irc-app conn))
         (network-name (clatter.core.config:network-config-name (irc-network-config conn)))
         (buffers (clatter.core.model:app-buffers app)))
    (loop for i from 0 below (length buffers)
          for buf = (aref buffers i)
          when (and buf
                    (string-equal (clatter.core.model:buffer-title buf) target)
                    (string-equal (clatter.core.model:buffer-network buf) network-name))
            return buf)))

(defun irc-find-or-create-buffer (conn target)
  "Find or create buffer for target. Thread-safe."
  (bordeaux-threads:with-lock-held (*buffer-creation-lock*)
    (or (irc-find-buffer conn target)
        (irc-create-channel-buffer-internal conn target))))

(defun irc-create-channel-buffer-internal (conn channel)
  "Create a new buffer for a channel. Must be called with *buffer-creation-lock* held."
  (let* ((app (irc-app conn))
         (kind (if (char= (char channel 0) #\#) :channel :query))
         (network-name (clatter.core.config:network-config-name (irc-network-config conn)))
         (buffers (clatter.core.model:app-buffers app))
         (buf (clatter.core.model:make-buffer :id (length buffers) :kind kind :title channel :network network-name)))
    ;; Add buffer directly
    (vector-push-extend buf buffers)
    ;; Mark dirty via ui-submit for thread safety on UI state
    (clatter.ui.tui:ui-submit
      (clatter.core.model:mark-dirty app :buflist))
    ;; Request channel modes for channels
    (when (and (> (length channel) 0) (char= (char channel 0) #\#))
      (irc-send conn (format nil "MODE ~a" channel)))
    buf))

(defun irc-create-channel-buffer (conn channel)
  "Create a new buffer for a channel. Thread-safe wrapper."
  (bordeaux-threads:with-lock-held (*buffer-creation-lock*)
    ;; Check if buffer already exists first
    (or (irc-find-buffer conn channel)
        (irc-create-channel-buffer-internal conn channel))))

(defun irc-read-loop (conn)
  "Main read loop for IRC connection."
  (handler-case
      (loop while (and (irc-stream conn)
                       (not (eq (irc-state conn) :disconnected)))
            do (let ((line (read-line (irc-stream conn) nil nil)))
                 (if line
                     (let ((msg (clatter.core.protocol:parse-irc-line
                                 (string-right-trim '(#\Return #\Linefeed) line))))
                       (irc-handle-message conn msg))
                     ;; EOF
                     (progn
                       (irc-log-system conn "Connection closed by server")
                       (setf (irc-state conn) :disconnected)
                       (return)))))
    (error (e)
      (irc-log-error conn "Read error: ~a" e)
      (setf (irc-state conn) :disconnected))))

(defun reconnect-delay (attempts)
  "Calculate reconnect delay with exponential backoff. Returns seconds."
  (min 300 (expt 2 (min attempts 8))))  ; 1, 2, 4, 8, 16, 32, 64, 128, 256, 300 max

(defun irc-connection-loop (conn)
  "Main connection loop with auto-reconnect support."
  (loop
    (setf (irc-sasl-state conn) nil
          (irc-cap-negotiating conn) nil
          (irc-ping-sent-time conn) nil
          (irc-last-activity conn) (get-universal-time))
    (cond
      ((irc-connect conn)
       ;; Connected successfully - reset attempts
       (setf (irc-reconnect-attempts conn) 0)
       (irc-register conn)
       (irc-read-loop conn)
       (irc-disconnect conn))
      (t
       ;; Connection failed
       (incf (irc-reconnect-attempts conn))))
    ;; Check if we should reconnect
    (unless (irc-reconnect-enabled conn)
      (irc-log-system conn "Reconnection disabled, staying disconnected")
      (return))
    ;; Calculate delay and wait
    (let ((delay (reconnect-delay (irc-reconnect-attempts conn))))
      (irc-log-system conn "Reconnecting in ~a seconds..." delay)
      (sleep delay))))

(defun start-irc-connection (app network-id network-config)
  "Start an IRC connection in a background thread."
  (let* ((network-name (clatter.core.config:network-config-name network-config))
         (conn (make-irc-connection app network-id network-config)))
    ;; Register connection in app
    (setf (gethash network-name (clatter.core.model:app-connections app)) conn)
    ;; Create server buffer for this network (directly, not via ui-submit,
    ;; since this may be called before TUI starts)
    (clatter.core.model:create-server-buffer app network-name)
    (setf (irc-thread conn)
          (bordeaux-threads:make-thread
           (lambda ()
             (irc-connection-loop conn))
           :name (format nil "irc-~a" network-name)))
    conn))

(defun irc-check-health (conn)
  "Check connection health. Call periodically from main thread.
   Sends a PING if idle too long, disconnects if PING times out."
  (when (eq (irc-state conn) :connected)
    (let* ((now (get-universal-time))
           (idle-time (- now (irc-last-activity conn)))
           (ping-timeout 120)   ; 2 minutes without activity -> send ping
           (pong-timeout 60))   ; 1 minute to respond to ping
      (cond
        ;; We sent a ping and it timed out
        ((and (irc-ping-sent-time conn)
              (> (- now (irc-ping-sent-time conn)) pong-timeout))
         (irc-log-error conn "Connection timed out (no PONG received)")
         (irc-disconnect conn))
        ;; Idle too long, send a ping
        ((and (null (irc-ping-sent-time conn))
              (> idle-time ping-timeout))
         (setf (irc-ping-sent-time conn) now)
         (irc-send conn (format nil "PING :clatter-~a" now)))))))
