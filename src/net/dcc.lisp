(in-package #:clatter.net.dcc)

;;; DCC (Direct Client-to-Client) Protocol Implementation
;;; Supports DCC CHAT and DCC SEND

;;;; ============================================================
;;;; Constants - use clatter.core.constants for DCC values
;;;; ============================================================

;;;; ============================================================
;;;; DCC Manager - tracks all DCC connections
;;;; ============================================================

(defvar *dcc-manager* nil
  "Global DCC manager instance.")

(defclass dcc-manager ()
  ((connections :initform (make-hash-table :test 'equal)
                :accessor dcc-connections
                :documentation "Hash table of id -> dcc-connection")
   (next-id :initform 1
            :accessor dcc-next-id
            :documentation "Next connection ID to assign")
   (app :initarg :app
        :accessor dcc-app
        :documentation "Reference to the main app")
   (irc-conn :initarg :irc-conn
             :accessor dcc-irc-conn
             :documentation "Reference to IRC connection for sending CTCP"))
  (:documentation "Manages all DCC connections."))

(defun make-dcc-manager (app irc-conn)
  "Create a new DCC manager."
  (setf *dcc-manager* (make-instance 'dcc-manager :app app :irc-conn irc-conn)))

(defun dcc-manager-add (manager connection)
  "Add a connection to the manager, assigning it an ID."
  (let ((id (dcc-next-id manager)))
    (setf (dcc-id connection) id)
    (setf (gethash id (dcc-connections manager)) connection)
    (incf (dcc-next-id manager))
    id))

(defun dcc-manager-remove (manager id)
  "Remove a connection from the manager."
  (remhash id (dcc-connections manager)))

(defun dcc-manager-find (manager id)
  "Find a connection by ID."
  (gethash id (dcc-connections manager)))

(defun dcc-manager-list (manager)
  "List all connections."
  (loop for conn being the hash-values of (dcc-connections manager)
        collect conn))

(defun dcc-manager-pending (manager)
  "List pending (awaiting accept/reject) connections."
  (loop for conn being the hash-values of (dcc-connections manager)
        when (eq (dcc-state conn) :pending)
          collect conn))

;;;; ============================================================
;;;; DCC Connection Base Class
;;;; ============================================================

(defclass dcc-connection ()
  ((id :initform nil
       :accessor dcc-id
       :documentation "Unique ID for this connection")
   (nick :initarg :nick
         :accessor dcc-nick
         :documentation "Remote user's nick")
   (remote-ip :initarg :remote-ip
              :initform nil
              :accessor dcc-remote-ip
              :documentation "Remote IP address (as integer or string)")
   (remote-port :initarg :remote-port
                :initform nil
                :accessor dcc-remote-port
                :documentation "Remote port number")
   (socket :initform nil
           :accessor dcc-socket
           :documentation "usocket socket object")
   (stream :initform nil
           :accessor dcc-stream
           :documentation "Stream for reading/writing")
   (state :initform :pending
          :accessor dcc-state
          :documentation "Connection state: :pending :connecting :active :closed :error")
   (direction :initarg :direction
              :accessor dcc-direction
              :documentation "Direction: :incoming (we receive offer) or :outgoing (we initiate)")
   (thread :initform nil
           :accessor dcc-thread
           :documentation "Background thread for this connection")
   (created-at :initform (get-universal-time)
               :accessor dcc-created-at
               :documentation "When this connection was created")
   (error-message :initform nil
                  :accessor dcc-error-message
                  :documentation "Error message if state is :error"))
  (:documentation "Base class for DCC connections."))

(defgeneric dcc-accept (connection manager)
  (:documentation "Accept a pending DCC connection."))

(defgeneric dcc-reject (connection manager)
  (:documentation "Reject a pending DCC connection."))

(defgeneric dcc-close (connection)
  (:documentation "Close an active DCC connection."))

(defgeneric dcc-type-string (connection)
  (:documentation "Return a string describing the connection type."))

(defgeneric dcc-status-string (connection)
  (:documentation "Return a human-readable status string."))

;;;; ============================================================
;;;; DCC CHAT
;;;; ============================================================

(defclass dcc-chat (dcc-connection)
  ((buffer :initarg :buffer
           :initform nil
           :accessor dcc-buffer
           :documentation "Associated chat buffer for messages"))
  (:documentation "DCC CHAT connection for direct messaging."))

(defmethod dcc-type-string ((conn dcc-chat))
  "CHAT")

(defmethod dcc-status-string ((conn dcc-chat))
  (format nil "DCC CHAT ~a ~a [~a]"
          (if (eq (dcc-direction conn) :incoming) "from" "to")
          (dcc-nick conn)
          (dcc-state conn)))

(defmethod dcc-accept ((conn dcc-chat) manager)
  "Accept incoming DCC CHAT - connect to remote host."
  (when (and (eq (dcc-state conn) :pending)
             (eq (dcc-direction conn) :incoming))
    (setf (dcc-state conn) :connecting)
    ;; Start connection thread
    (setf (dcc-thread conn)
          (bt:make-thread
           (lambda () (dcc-chat-connect conn manager))
           :name (format nil "dcc-chat-~a" (dcc-nick conn))))
    t))

(defmethod dcc-reject ((conn dcc-chat) manager)
  "Reject incoming DCC CHAT."
  (when (eq (dcc-state conn) :pending)
    (setf (dcc-state conn) :closed)
    (dcc-manager-remove manager (dcc-id conn))
    (dcc-log-system manager "DCC CHAT from ~a rejected" (dcc-nick conn))
    t))

(defmethod dcc-close ((conn dcc-chat))
  "Close DCC CHAT connection."
  (setf (dcc-state conn) :closed)
  (when (dcc-stream conn)
    (ignore-errors (close (dcc-stream conn))))
  (when (dcc-socket conn)
    (ignore-errors (usocket:socket-close (dcc-socket conn))))
  (setf (dcc-stream conn) nil
        (dcc-socket conn) nil))

(defun dcc-chat-connect (conn manager)
  "Connect to remote DCC CHAT host (runs in thread)."
  (handler-case
      (let* ((ip-str (ip-integer-to-string (dcc-remote-ip conn)))
             (port (dcc-remote-port conn))
             (socket (usocket:socket-connect ip-str port :element-type '(unsigned-byte 8)))
             (stream (usocket:socket-stream socket)))
        (setf (dcc-socket conn) socket
              (dcc-stream conn) stream
              (dcc-state conn) :active)
        ;; Create buffer for this chat
        (clatter.ui.tui:ui-submit
          (let ((buf (create-dcc-chat-buffer manager conn)))
            (setf (dcc-buffer conn) buf)
            (dcc-log-system manager "DCC CHAT with ~a established" (dcc-nick conn))))
        ;; Start read loop
        (dcc-chat-read-loop conn manager))
    (error (e)
      (setf (dcc-state conn) :error
            (dcc-error-message conn) (format nil "~a" e))
      (dcc-log-system manager "DCC CHAT connection to ~a failed: ~a" 
                      (dcc-nick conn) e))))

(defun dcc-chat-read-loop (conn manager)
  "Read loop for DCC CHAT (runs in thread)."
  (handler-case
      (let ((stream (dcc-stream conn)))
        (loop while (eq (dcc-state conn) :active)
              for line = (read-line-from-binary-stream stream)
              while line
              do (clatter.ui.tui:ui-submit
                   (deliver-dcc-chat-message manager conn line))))
    (end-of-file ()
      (dcc-log-system manager "DCC CHAT with ~a closed by remote" (dcc-nick conn)))
    (error (e)
      (unless (eq (dcc-state conn) :closed)
        (dcc-log-system manager "DCC CHAT error with ~a: ~a" (dcc-nick conn) e))))
  (dcc-close conn))

(defun dcc-chat-send (conn text)
  "Send a line of text over DCC CHAT."
  (when (and (eq (dcc-state conn) :active)
             (dcc-stream conn))
    (handler-case
        (let ((stream (dcc-stream conn)))
          (write-line-to-binary-stream stream text)
          (force-output stream)
          t)
      (error (e)
        (setf (dcc-state conn) :error
              (dcc-error-message conn) (format nil "Send error: ~a" e))
        nil))))

;;;; ============================================================
;;;; DCC SEND (File Transfer)
;;;; ============================================================

(defclass dcc-send (dcc-connection)
  ((filename :initarg :filename
             :accessor dcc-filename
             :documentation "Name of file being transferred")
   (filepath :initarg :filepath
             :initform nil
             :accessor dcc-filepath
             :documentation "Full path to local file")
   (filesize :initarg :filesize
             :accessor dcc-filesize
             :documentation "Size of file in bytes")
   (bytes-transferred :initform 0
                      :accessor dcc-bytes-transferred
                      :documentation "Bytes transferred so far")
   (file-stream :initform nil
                :accessor dcc-file-stream
                :documentation "File stream for reading/writing"))
  (:documentation "DCC SEND connection for file transfers."))

(defmethod dcc-type-string ((conn dcc-send))
  "SEND")

(defmethod dcc-status-string ((conn dcc-send))
  (let ((pct (if (> (dcc-filesize conn) 0)
                 (floor (* 100 (dcc-bytes-transferred conn)) (dcc-filesize conn))
                 0)))
    (format nil "DCC ~a ~a ~a (~a/~a bytes, ~d%) [~a]"
            (if (eq (dcc-direction conn) :incoming) "RECV" "SEND")
            (if (eq (dcc-direction conn) :incoming) "from" "to")
            (dcc-nick conn)
            (dcc-bytes-transferred conn)
            (dcc-filesize conn)
            pct
            (dcc-state conn))))

(defmethod dcc-accept ((conn dcc-send) manager)
  "Accept incoming DCC SEND - connect and receive file."
  (when (and (eq (dcc-state conn) :pending)
             (eq (dcc-direction conn) :incoming))
    (setf (dcc-state conn) :connecting)
    ;; Start receive thread
    (setf (dcc-thread conn)
          (bt:make-thread
           (lambda () (dcc-receive-file conn manager))
           :name (format nil "dcc-recv-~a" (dcc-filename conn))))
    t))

(defmethod dcc-reject ((conn dcc-send) manager)
  "Reject incoming DCC SEND."
  (when (eq (dcc-state conn) :pending)
    (setf (dcc-state conn) :closed)
    (dcc-manager-remove manager (dcc-id conn))
    (dcc-log-system manager "DCC SEND ~a from ~a rejected" 
                    (dcc-filename conn) (dcc-nick conn))
    t))

(defmethod dcc-close ((conn dcc-send))
  "Close DCC SEND connection."
  (setf (dcc-state conn) :closed)
  (when (dcc-file-stream conn)
    (ignore-errors (close (dcc-file-stream conn))))
  (when (dcc-stream conn)
    (ignore-errors (close (dcc-stream conn))))
  (when (dcc-socket conn)
    (ignore-errors (usocket:socket-close (dcc-socket conn))))
  (setf (dcc-file-stream conn) nil
        (dcc-stream conn) nil
        (dcc-socket conn) nil))

(defun dcc-receive-file (conn manager)
  "Receive a file via DCC SEND (runs in thread)."
  (handler-case
      (let* ((ip-str (ip-integer-to-string (dcc-remote-ip conn)))
             (port (dcc-remote-port conn)))
        (dcc-log-system manager "DCC RECV connecting to ~a:~a" ip-str port)
        (let* ((socket (usocket:socket-connect ip-str port 
                                               :element-type '(unsigned-byte 8)))
               (raw-stream (usocket:socket-stream socket))
               (download-dir (get-download-directory))
               (filepath (merge-pathnames (dcc-filename conn) download-dir))
               (file-stream (open filepath :direction :output 
                                          :element-type '(unsigned-byte 8)
                                          :if-exists :supersede)))
          (setf (dcc-socket conn) socket
                (dcc-stream conn) raw-stream
                (dcc-filepath conn) filepath
                (dcc-file-stream conn) file-stream
                (dcc-state conn) :active)
          (dcc-log-system manager "DCC RECV ~a from ~a started (expecting ~a bytes)" 
                          (dcc-filename conn) (dcc-nick conn) (dcc-filesize conn))
          ;; Receive loop - read available bytes using the underlying fd-stream
          (let ((buffer (make-array clatter.core.constants:+dcc-buffer-size+ :element-type '(unsigned-byte 8)))
                (total-received 0)
                (filesize (dcc-filesize conn)))
            (block receive-loop
              (loop while (< total-received filesize)
                    do ;; Wait for data with timeout
                       (unless (usocket:wait-for-input socket :timeout 30 :ready-only t)
                         (dcc-log-system manager "DCC RECV timeout waiting for data")
                         (return-from receive-loop))
                       ;; Read available bytes one at a time (read-byte blocks properly)
                       (let ((bytes-read 0)
                             (max-read (min clatter.core.constants:+dcc-buffer-size+ (- filesize total-received))))
                         ;; Read first byte (blocks until available)
                         (let ((byte (read-byte raw-stream nil nil)))
                           (unless byte
                             (dcc-log-system manager "DCC RECV got EOF after ~a bytes" total-received)
                             (return-from receive-loop))
                           (setf (aref buffer 0) byte)
                           (incf bytes-read))
                         ;; Read remaining available bytes without blocking
                         (loop while (and (< bytes-read max-read)
                                          (listen raw-stream))
                               for byte = (read-byte raw-stream nil nil)
                               while byte
                               do (setf (aref buffer bytes-read) byte)
                                  (incf bytes-read))
                         ;; Write to file
                         (write-sequence buffer file-stream :end bytes-read)
                         (force-output file-stream)
                         (incf total-received bytes-read)
                         (setf (dcc-bytes-transferred conn) total-received)
                         ;; Send ACK (4-byte network order)
                         (send-dcc-ack raw-stream total-received))))
            (close file-stream)
            (setf (dcc-file-stream conn) nil)
            (dcc-log-system manager "DCC RECV ~a complete (~a bytes)" 
                            (dcc-filename conn) total-received))))
    (error (e)
      (setf (dcc-state conn) :error
            (dcc-error-message conn) (format nil "~a" e))
      (dcc-log-system manager "DCC RECV ~a failed: ~a" (dcc-filename conn) e)))
  (dcc-close conn))

(defun dcc-send-file (conn manager)
  "Send a file via DCC SEND (runs in thread after remote connects)."
  (handler-case
      (let* ((file-stream (open (dcc-filepath conn) :direction :input
                                                   :element-type '(unsigned-byte 8)))
             (stream (dcc-stream conn))
             (buffer (make-array clatter.core.constants:+dcc-buffer-size+ :element-type '(unsigned-byte 8)))
             (total-sent 0))
        (setf (dcc-file-stream conn) file-stream
              (dcc-state conn) :active)
        (dcc-log-system manager "DCC SEND ~a to ~a started" 
                        (dcc-filename conn) (dcc-nick conn))
        ;; Send loop - send all data without waiting for ACKs
        ;; (many clients don't send ACKs until after receiving all data)
        (loop for bytes-read = (read-sequence buffer file-stream)
              while (> bytes-read 0)
              do (write-sequence buffer stream :end bytes-read)
                 (incf total-sent bytes-read)
                 (setf (dcc-bytes-transferred conn) total-sent))
        (force-output stream)
        (close file-stream)
        (setf (dcc-file-stream conn) nil)
        (dcc-log-system manager "DCC SEND ~a complete (~a bytes)" 
                        (dcc-filename conn) total-sent))
    (error (e)
      (setf (dcc-state conn) :error
            (dcc-error-message conn) (format nil "~a" e))
      (dcc-log-system manager "DCC SEND ~a failed: ~a" (dcc-filename conn) e)))
  (dcc-close conn))

;;;; ============================================================
;;;; Initiating DCC connections (outgoing)
;;;; ============================================================

(defun dcc-initiate-chat (manager nick)
  "Initiate a DCC CHAT with nick."
  (let* ((local-ip (get-local-ip))
         (port (find-available-port)))
    (handler-case
        (let* ((listener (usocket:socket-listen usocket:*wildcard-host* port :reuse-address t))
               (conn (make-instance 'dcc-chat
                                    :nick nick
                                    :direction :outgoing)))
          (setf (dcc-socket conn) listener)
          (dcc-manager-add manager conn)
          ;; Send CTCP DCC CHAT
          (let* ((ip-int (ip-string-to-integer local-ip))
                 (ctcp-msg (format nil "~CDCC CHAT chat ~a ~a~C" 
                                   (code-char 1) ip-int port (code-char 1))))
            (clatter.net.irc:irc-send (dcc-irc-conn manager)
                                      (clatter.core.protocol:irc-privmsg nick ctcp-msg)))
          (dcc-log-system manager "DCC CHAT request sent to ~a (port ~a, ip ~a)" nick port local-ip)
          ;; Start listener thread
          (setf (dcc-thread conn)
                (bt:make-thread
                 (lambda () (dcc-chat-listen conn manager listener))
                 :name (format nil "dcc-chat-listen-~a" nick)))
          conn)
      (error (e)
        (dcc-log-system manager "Failed to initiate DCC CHAT: ~a" e)
        nil))))

(defun dcc-chat-listen (conn manager listener)
  "Wait for incoming connection on DCC CHAT listener."
  (handler-case
      (let ((socket (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
        (usocket:socket-close listener)
        (setf (dcc-socket conn) socket
              (dcc-stream conn) (usocket:socket-stream socket)
              (dcc-state conn) :active)
        ;; Create buffer
        (clatter.ui.tui:ui-submit
          (let ((buf (create-dcc-chat-buffer manager conn)))
            (setf (dcc-buffer conn) buf)
            (dcc-log-system manager "DCC CHAT with ~a established" (dcc-nick conn))))
        ;; Start read loop
        (dcc-chat-read-loop conn manager))
    (error (e)
      (setf (dcc-state conn) :error
            (dcc-error-message conn) (format nil "~a" e))
      (dcc-log-system manager "DCC CHAT listen failed: ~a" e))))

(defun dcc-initiate-send (manager nick filepath &optional irc-conn)
  "Initiate a DCC SEND to nick. If irc-conn is provided, use it instead of manager's connection."
  (handler-case
      (progn
        (unless (probe-file filepath)
          (dcc-log-system manager "File not found: ~a" filepath)
          (return-from dcc-initiate-send nil))
        (let* ((local-ip (get-local-ip))
               (port (find-available-port))
               (filename (file-namestring filepath))
               (filesize (with-open-file (f filepath) (file-length f))))
          (let* ((listener (usocket:socket-listen usocket:*wildcard-host* port :reuse-address t))
                 (conn (make-instance 'dcc-send
                                      :nick nick
                                      :direction :outgoing
                                      :filename filename
                                      :filepath filepath
                                      :filesize filesize)))
            (setf (dcc-socket conn) listener)
            (dcc-manager-add manager conn)
            ;; Send CTCP DCC SEND - use provided connection or fall back to manager's
            (let* ((ip-int (ip-string-to-integer local-ip))
                   (ctcp-msg (format nil "~CDCC SEND ~a ~a ~a ~a~C"
                                     (code-char 1) filename ip-int port filesize (code-char 1)))
                   (full-msg (clatter.core.protocol:irc-privmsg nick ctcp-msg))
                   (send-conn (or irc-conn (dcc-irc-conn manager))))
              (when send-conn
                (clatter.net.irc:irc-send send-conn full-msg)))
            (dcc-log-system manager "DCC SEND ~a to ~a (~a bytes)" filename nick filesize)
            ;; Start listener thread
            (setf (dcc-thread conn)
                  (bt:make-thread
                   (lambda () (dcc-send-listen conn manager listener))
                   :name (format nil "dcc-send-listen-~a" filename)))
            conn)))
    (error (e)
      (dcc-log-system manager "Failed to initiate DCC SEND: ~a" e)
      nil)))

(defun dcc-send-listen (conn manager listener)
  "Wait for incoming connection on DCC SEND listener."
  (dcc-log-system manager "DCC SEND waiting for connection on port ~a..." 
                  (usocket:get-local-port listener))
  (handler-case
      (let ((socket (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
        (dcc-log-system manager "DCC SEND connection accepted from ~a" 
                        (usocket:get-peer-address socket))
        (usocket:socket-close listener)
        (setf (dcc-socket conn) socket
              (dcc-stream conn) (usocket:socket-stream socket))
        ;; Start sending
        (dcc-send-file conn manager))
    (error (e)
      (setf (dcc-state conn) :error
            (dcc-error-message conn) (format nil "~a" e))
      (dcc-log-system manager "DCC SEND listen failed: ~a" e))))

;;;; ============================================================
;;;; Handling incoming DCC offers (from CTCP)
;;;; ============================================================

(defun dcc-handle-offer (manager nick dcc-type args)
  "Handle an incoming DCC offer from CTCP.
   DCC-TYPE is \"CHAT\" or \"SEND\".
   ARGS is the rest of the DCC line after the type."
  (cond
    ((string-equal dcc-type "CHAT")
     (dcc-handle-chat-offer manager nick args))
    ((string-equal dcc-type "SEND")
     (dcc-handle-send-offer manager nick args))
    (t
     (dcc-log-system manager "Unknown DCC type from ~a: ~a" nick dcc-type))))

(defun dcc-handle-chat-offer (manager nick args)
  "Handle incoming DCC CHAT offer.
   ARGS format: chat <ip> <port>"
  (let* ((parts (split-string args))
         (ip-int (safe-parse-integer (second parts)))
         (port (safe-parse-integer (third parts)))
         (conn (make-instance 'dcc-chat
                              :nick nick
                              :direction :incoming
                              :remote-ip ip-int
                              :remote-port port)))
    (dcc-manager-add manager conn)
    (dcc-notify-offer manager "DCC CHAT offer from ~a (use /dcc accept ~a or /dcc reject ~a)"
                      nick (dcc-id conn) (dcc-id conn))
    conn))

(defun dcc-handle-send-offer (manager nick args)
  "Handle incoming DCC SEND offer.
   ARGS format: <filename> <ip> <port> <filesize>"
  (let* ((parts (split-string args))
         (filename (first parts))
         (ip-int (safe-parse-integer (second parts)))
         (port (safe-parse-integer (third parts)))
         (filesize (safe-parse-integer (fourth parts)))
         (conn (make-instance 'dcc-send
                              :nick nick
                              :direction :incoming
                              :remote-ip ip-int
                              :remote-port port
                              :filename filename
                              :filesize filesize)))
    (dcc-manager-add manager conn)
    (dcc-notify-offer manager "DCC SEND offer: ~a from ~a (~a bytes) (use /dcc accept ~a)"
                      filename nick filesize (dcc-id conn))
    conn))

;;;; ============================================================
;;;; Utility Functions
;;;; ============================================================

(defun ip-integer-to-string (ip-int)
  "Convert a 32-bit integer IP to dotted string format."
  (format nil "~d.~d.~d.~d"
          (ldb (byte 8 24) ip-int)
          (ldb (byte 8 16) ip-int)
          (ldb (byte 8 8) ip-int)
          (ldb (byte 8 0) ip-int)))

(defun ip-string-to-integer (ip-str)
  "Convert dotted IP string to 32-bit integer."
  (handler-case
      (let ((parts (mapcar #'parse-integer (split-string ip-str #\.))))
        (if (and (= (length parts) 4)
                 (every #'integerp parts))
            (+ (ash (first parts) 24)
               (ash (second parts) 16)
               (ash (third parts) 8)
               (fourth parts))
            0))
    (error () 0)))

(defvar *dcc-local-ip* nil
  "Cached local IP address for DCC. Set via /dcc ip or auto-detected.")

(defun get-local-ip ()
  "Get local IP address for DCC.
   Returns cached IP if set, otherwise tries to detect it."
  (or *dcc-local-ip*
      (detect-local-ip)
      "127.0.0.1"))

(defun detect-local-ip ()
  "Try to detect local IP by connecting to a remote host.
   This works by creating a UDP socket and checking its local address."
  (handler-case
      (let ((socket (usocket:socket-connect "8.8.8.8" 53 
                                            :protocol :datagram
                                            :element-type '(unsigned-byte 8))))
        (unwind-protect
            (let ((local-addr (usocket:get-local-name socket)))
              (when local-addr
                (usocket:host-to-hostname local-addr)))
          (usocket:socket-close socket)))
    (error () nil)))

(defun set-dcc-ip (ip-string)
  "Manually set the DCC IP address."
  (setf *dcc-local-ip* ip-string))

(defun find-available-port ()
  "Find an available port for DCC listening."
  (loop for port from clatter.core.constants:+dcc-port-range-start+ to clatter.core.constants:+dcc-port-range-end+
        do (handler-case
               (let ((socket (usocket:socket-listen usocket:*wildcard-host* port)))
                 (usocket:socket-close socket)
                 (return port))
             (error () nil))
        finally (error "No available port found for DCC")))

(defun get-download-directory ()
  "Get the directory for DCC downloads."
  (let ((dir (merge-pathnames "Downloads/CLatter/" (user-homedir-pathname))))
    (ensure-directories-exist dir)
    dir))

(defun split-string (string &optional (delimiter #\Space))
  "Split string by delimiter."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun safe-parse-integer (string)
  "Parse integer from string, returning nil on failure."
  (ignore-errors (cl:parse-integer string)))

(defun read-line-from-binary-stream (stream)
  "Read a line from a binary stream."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) 
                            :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          while (and byte (/= byte (char-code #\Newline)))
          do (vector-push-extend byte bytes)
          finally (when (= (length bytes) 0)
                    (return nil)))
    ;; Strip CR if present
    (when (and (> (length bytes) 0)
               (= (aref bytes (1- (length bytes))) (char-code #\Return)))
      (decf (fill-pointer bytes)))
    (flexi-streams:octets-to-string bytes :external-format :utf-8)))

(defun write-line-to-binary-stream (stream text)
  "Write a line to a binary stream."
  (let ((bytes (flexi-streams:string-to-octets text :external-format :utf-8)))
    (write-sequence bytes stream)
    (write-byte (char-code #\Newline) stream)))

(defun send-dcc-ack (stream bytes-received)
  "Send DCC ACK (4-byte network order integer)."
  (let ((ack (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref ack 0) (ldb (byte 8 24) bytes-received)
          (aref ack 1) (ldb (byte 8 16) bytes-received)
          (aref ack 2) (ldb (byte 8 8) bytes-received)
          (aref ack 3) (ldb (byte 8 0) bytes-received))
    (write-sequence ack stream)
    (force-output stream)))

(defun receive-dcc-ack (stream)
  "Receive DCC ACK (4-byte network order integer)."
  (let ((ack (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence ack stream)
    (+ (ash (aref ack 0) 24)
       (ash (aref ack 1) 16)
       (ash (aref ack 2) 8)
       (aref ack 3))))

;;;; ============================================================
;;;; Integration with UI
;;;; ============================================================

(defun create-dcc-chat-buffer (manager conn)
  "Create a buffer for DCC CHAT."
  (let* ((app (dcc-app manager))
         (title (format nil "DCC:~a" (dcc-nick conn)))
         (buf (clatter.core.model:make-buffer :id -1 :kind :dcc-chat :title title)))
    (let ((buffers (clatter.core.model:app-buffers app)))
      (setf (clatter.core.model:buffer-id buf) (length buffers))
      (vector-push-extend buf buffers)
      (clatter.core.model:mark-dirty app :buflist))
    buf))

(defun find-dcc-connection-for-buffer (manager buf)
  "Find the DCC connection associated with a buffer."
  (loop for conn being the hash-values of (dcc-connections manager)
        when (eq (dcc-buffer conn) buf)
        return conn))

(defun deliver-dcc-chat-message (manager conn text)
  "Deliver a received DCC CHAT message to the buffer."
  (let ((buf (dcc-buffer conn))
        (app (dcc-app manager)))
    (when buf
      (clatter.core.dispatch:deliver-message
       app buf
       (clatter.core.model:make-message :level :chat
                                        :nick (dcc-nick conn)
                                        :text text)))))

(defun dcc-log-system (manager format-string &rest args)
  "Log a DCC system message to the current buffer."
  (let* ((app (dcc-app manager))
         (buf (clatter.core.model:current-buffer app))
         (text (apply #'format nil format-string args)))
    (clatter.ui.tui:ui-submit
      (when buf
        (clatter.core.dispatch:deliver-message
         app buf
         (clatter.core.model:make-message :level :system :text text))))))

(defun dcc-notify-offer (manager format-string &rest args)
  "Notify user of DCC offer - shows in current buffer with highlight."
  (let* ((app (dcc-app manager))
         (current-buf (clatter.core.model:current-buffer app))
         (text (apply #'format nil format-string args))
         (msg (clatter.core.model:make-message :level :system :text text :highlight t)))
    (clatter.ui.tui:ui-submit
      (when current-buf
        (clatter.core.dispatch:deliver-message app current-buf msg)
        (incf (clatter.core.model:buffer-highlight-count current-buf))
        (clatter.core.model:mark-dirty app :buflist)))))
