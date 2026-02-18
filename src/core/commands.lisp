(in-package #:clatter.core.commands)

;;; Command parsing and execution
;;; Commands start with / and are parsed here

(defvar *current-connection* nil "Current active IRC connection")
(defvar *current-config* nil "Current config object for saving autojoin etc")

(defun parse-command (line)
  "Parse a command line. Returns (command . args) or nil if not a command."
  (when (and (> (length line) 0) (char= (char line 0) #\/))
    (let* ((space-pos (position #\Space line))
           (cmd (string-upcase (subseq line 1 (or space-pos (length line)))))
           (args (if space-pos
                     (string-trim '(#\Space) (subseq line (1+ space-pos)))
                     "")))
      (cons cmd args))))

(defun split-first-word (str)
  "Split string into first word and rest."
  (let ((space-pos (position #\Space str)))
    (if space-pos
        (values (subseq str 0 space-pos)
                (string-trim '(#\Space) (subseq str (1+ space-pos))))
        (values str ""))))

(defun execute-command (app conn cmd args)
  "Execute a parsed command. Returns t if handled, nil otherwise.
   All commands are now dispatched via CLOS - see command-classes.lisp"
  (let ((command (find-command cmd)))
    (when command
      (return-from execute-command (execute-cmd command app conn args))))
  ;; Unknown command
  nil)

(defun get-current-network-config (app)
  "Get the network config for the current buffer's connection."
  (let ((conn (clatter.core.model:get-current-connection app)))
    (when (and conn *current-config*)
      (clatter.net.irc:irc-network-config conn))))

(defun handle-log-command (app args)
  "Handle /log command: view recent logs or search.
   /log - show recent logs for current buffer
   /log search <pattern> - search logs for pattern
   /log list - list all logged targets
   /log export [text|json|html] [path] - export logs"
  (let* ((buf (clatter.core.model:active-buffer app))
         (target (if buf (clatter.core.model:buffer-title buf) ""))
         (kind (if buf (clatter.core.model:buffer-kind buf) :server))
         (network clatter.core.logging:*current-network*))
    (cond
      ;; /log export [format] [path] - export logs
      ((and (>= (length args) 6)
            (string-equal (string-upcase (subseq args 0 6)) "EXPORT"))
       (if (member kind '(:channel :query))
           (let* ((rest (string-trim " " (subseq args 6)))
                  (parts (uiop:split-string rest :separator " "))
                  (format-str (string-upcase (or (first parts) "TEXT")))
                  (format-key (cond ((string= format-str "JSON") :json)
                                    ((string= format-str "HTML") :html)
                                    (t :text)))
                  (ext (case format-key (:json "json") (:html "html") (t "txt")))
                  (default-path (merge-pathnames 
                                 (format nil "~a-export.~a" 
                                         (clatter.core.logging::sanitize-target target) ext)
                                 (user-homedir-pathname)))
                  (output-path (if (second parts)
                                   (pathname (second parts))
                                   default-path))
                  (count (clatter.core.logging:export-logs network target format-key output-path)))
             (clatter.ui.tui:ui-submit
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message 
                 :level :system :nick "*log*"
                 :text (if count
                           (format nil "Exported ~d lines to ~a" count (namestring output-path))
                           "No logs to export")))))
           (clatter.ui.tui:ui-submit
             (clatter.core.dispatch:deliver-message
              app buf
              (clatter.core.model:make-message :level :error :nick "*"
                                               :text "Use /log export in a channel or query buffer")))))
      ;; /log list - show all logged targets
      ((and (> (length args) 0)
            (string-equal (string-upcase (subseq args 0 (min 4 (length args)))) "LIST"))
       (let ((targets (clatter.core.logging:list-logged-targets network)))
         (clatter.ui.tui:ui-submit
           (clatter.core.dispatch:deliver-message
            app buf
            (clatter.core.model:make-message :level :system :nick "*log*"
                                             :text (if targets
                                                       (format nil "Logged targets: ~{~a~^, ~}" targets)
                                                       "No logs found"))))))
      ;; /log search <pattern> - search logs
      ((and (> (length args) 6)
            (string-equal (string-upcase (subseq args 0 6)) "SEARCH"))
       (let ((pattern (string-trim " " (subseq args 6))))
         (if (and (member kind '(:channel :query)) (> (length pattern) 0))
             (let ((results (clatter.core.logging:search-logs network target pattern)))
               (clatter.ui.tui:ui-submit
                 (if results
                     (progn
                       (clatter.core.dispatch:deliver-message
                        app buf
                        (clatter.core.model:make-message :level :system :nick "*log*"
                                                         :text (format nil "Search results for '~a' (~d matches):"
                                                                       pattern (length results))))
                       (dolist (result results)
                         (clatter.core.dispatch:deliver-message
                          app buf
                          (clatter.core.model:make-message :level :system :nick (car result)
                                                           :text (cdr result)))))
                     (clatter.core.dispatch:deliver-message
                      app buf
                      (clatter.core.model:make-message :level :system :nick "*log*"
                                                       :text (format nil "No matches for '~a'" pattern))))))
             (clatter.ui.tui:ui-submit
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message :level :error :nick "*"
                                                 :text "Usage: /log search <pattern> (in a channel/query buffer)"))))))
      ;; /log - show recent logs for current buffer
      ((member kind '(:channel :query))
       (let ((lines (clatter.core.logging:read-recent-logs network target 50)))
         (clatter.ui.tui:ui-submit
           (if lines
               (progn
                 (clatter.core.dispatch:deliver-message
                  app buf
                  (clatter.core.model:make-message :level :system :nick "*log*"
                                                   :text (format nil "--- Recent logs for ~a ---" target)))
                 (dolist (line lines)
                   (clatter.core.dispatch:deliver-message
                    app buf
                    (clatter.core.model:make-message :level :system :nick "" :text line)))
                 (clatter.core.dispatch:deliver-message
                  app buf
                  (clatter.core.model:make-message :level :system :nick "*log*"
                                                   :text "--- End of logs ---")))
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message :level :system :nick "*log*"
                                                 :text (format nil "No logs found for ~a" target)))))))
      ;; Not in a channel/query buffer
      (t
       (clatter.ui.tui:ui-submit
         (clatter.core.dispatch:deliver-message
          app buf
          (clatter.core.model:make-message :level :error :nick "*"
                                           :text "Use /log in a channel or query buffer, or /log list")))))))

(defun handle-ignore-command (app args)
  "Handle /ignore command.
   /ignore - list ignored nicks
   /ignore nick - toggle ignore for nick"
  (let ((buf (clatter.core.model:current-buffer app)))
    (if (or (null args) (= (length (string-trim " " args)) 0))
        ;; List ignored nicks
        (let ((ignored (clatter.core.model:list-ignored app)))
          (clatter.ui.tui:ui-submit
            (clatter.core.dispatch:deliver-message
             app buf
             (clatter.core.model:make-message 
              :level :system :nick "*"
              :text (if ignored
                        (format nil "Ignored: ~{~a~^, ~}" ignored)
                        "No nicks ignored")))))
        ;; Toggle ignore for nick
        (let ((nick (string-trim " " args)))
          (if (clatter.core.model:ignored-p app nick)
              (progn
                (clatter.core.model:unignore-nick app nick)
                (clatter.ui.tui:ui-submit
                  (clatter.core.dispatch:deliver-message
                   app buf
                   (clatter.core.model:make-message :level :system :nick "*"
                                                    :text (format nil "Unignored: ~a" nick)))))
              (progn
                (clatter.core.model:ignore-nick app nick)
                (clatter.ui.tui:ui-submit
                  (clatter.core.dispatch:deliver-message
                   app buf
                   (clatter.core.model:make-message :level :system :nick "*"
                                                    :text (format nil "Ignored: ~a" nick))))))))))

(defun add-to-autojoin (app channel)
  "Add a channel to the autojoin list and save config."
  (let ((net-cfg (get-current-network-config app)))
    (when net-cfg
      (let ((autojoin (clatter.core.config:network-config-autojoin net-cfg)))
        (unless (member channel autojoin :test #'string-equal)
          (setf (clatter.core.config:network-config-autojoin net-cfg)
                (append autojoin (list channel)))
          (clatter.core.config:save-config *current-config*)
          (clatter.ui.tui:ui-submit
            (clatter.core.dispatch:deliver-message
             app (clatter.core.model:current-buffer app)
             (clatter.core.model:make-message :level :system :nick "*"
                                              :text (format nil "Added ~a to autojoin" channel)))))))))

(defun remove-from-autojoin (app channel)
  "Remove a channel from the autojoin list and save config."
  (let ((net-cfg (get-current-network-config app)))
    (when net-cfg
      (let ((autojoin (clatter.core.config:network-config-autojoin net-cfg)))
        (if (member channel autojoin :test #'string-equal)
            (progn
              (setf (clatter.core.config:network-config-autojoin net-cfg)
                    (remove channel autojoin :test #'string-equal))
              (clatter.core.config:save-config *current-config*)
              (clatter.ui.tui:ui-submit
                (clatter.core.dispatch:deliver-message
                 app (clatter.core.model:current-buffer app)
                 (clatter.core.model:make-message :level :system :nick "*"
                                                  :text (format nil "Removed ~a from autojoin" channel)))))
            (clatter.ui.tui:ui-submit
              (clatter.core.dispatch:deliver-message
               app (clatter.core.model:current-buffer app)
               (clatter.core.model:make-message :level :error :nick "*"
                                                :text (format nil "~a is not in autojoin list" channel)))))))))

(defun list-autojoin (app)
  "List all channels in the autojoin list."
  (let ((net-cfg (get-current-network-config app)))
    (clatter.ui.tui:ui-submit
      (if net-cfg
          (let ((autojoin (clatter.core.config:network-config-autojoin net-cfg)))
            (clatter.core.dispatch:deliver-message
             app (clatter.core.model:current-buffer app)
             (clatter.core.model:make-message :level :system :nick "*"
                                              :text (if autojoin
                                                        (format nil "Autojoin: ~{~a~^, ~}" autojoin)
                                                        "Autojoin list is empty"))))
          (clatter.core.dispatch:deliver-message
           app (clatter.core.model:current-buffer app)
           (clatter.core.model:make-message :level :error :nick "*"
                                            :text "No active connection"))))))

(defun handle-autojoin-command (app args)
  "Handle /autojoin command with subcommands: list, add, remove."
  (if (= (length args) 0)
      (list-autojoin app)
      (multiple-value-bind (subcmd rest) (split-first-word args)
        (let ((subcmd (string-upcase subcmd)))
          (cond
            ((string= subcmd "LIST")
             (list-autojoin app))
            ((string= subcmd "ADD")
             (if (> (length rest) 0)
                 (multiple-value-bind (channel ignored) (split-first-word rest)
                   (declare (ignore ignored))
                   (add-to-autojoin app channel))
                 (clatter.ui.tui:ui-submit
                   (clatter.core.dispatch:deliver-message
                    app (clatter.core.model:current-buffer app)
                    (clatter.core.model:make-message :level :error :nick "*"
                                                     :text "Usage: /autojoin add #channel")))))
            ((string= subcmd "REMOVE")
             (if (> (length rest) 0)
                 (multiple-value-bind (channel ignored) (split-first-word rest)
                   (declare (ignore ignored))
                   (remove-from-autojoin app channel))
                 (clatter.ui.tui:ui-submit
                   (clatter.core.dispatch:deliver-message
                    app (clatter.core.model:current-buffer app)
                    (clatter.core.model:make-message :level :error :nick "*"
                                                     :text "Usage: /autojoin remove #channel")))))
            (t
             (clatter.ui.tui:ui-submit
               (clatter.core.dispatch:deliver-message
                app (clatter.core.model:current-buffer app)
                (clatter.core.model:make-message :level :error :nick "*"
                                                 :text "Usage: /autojoin [list|add|remove] [#channel]")))))))))


;; CrafterBin URL - use clatter.core.constants:+crafterbin-url+

(defun handle-crafterbin-command (app args)
  "Handle /crafterbin <file> - upload file to crafterbin and copy URL to clipboard."
  (let ((buf (clatter.core.model:current-buffer app)))
    (if (= (length args) 0)
        (clatter.ui.tui:ui-submit
          (clatter.core.dispatch:deliver-message
           app buf
           (clatter.core.model:make-message :level :error :nick "*"
                                            :text "Usage: /crafterbin <filepath>")))
        ;; Expand path and check if file exists
        (let ((filepath (uiop:native-namestring (uiop:parse-native-namestring args))))
          (cond
            ((not (probe-file filepath))
             (clatter.ui.tui:ui-submit
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message :level :error :nick "*"
                                                 :text (format nil "File not found: ~a" filepath)))))
            ((not (zerop (nth-value 2 (uiop:run-program "which curl" :output nil :ignore-error-status t))))
             (clatter.ui.tui:ui-submit
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message :level :error :nick "*"
                                                 :text "curl not found in PATH"))))
            (t
             ;; Run curl in background thread
             (clatter.ui.tui:ui-submit
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message :level :system :nick "*crafterbin*"
                                                 :text (format nil "Uploading ~a..." (file-namestring filepath)))))
             (bt:make-thread
              (lambda ()
                (crafterbin-upload-file app buf filepath))
              :name "crafterbin-upload")))))))

(defun crafterbin-upload-file (app buf filepath)
  "Upload file to crafterbin and copy URL to clipboard."
  (handler-case
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program 
           (list "curl" "-s" "-X" "POST" "-F" (format nil "file=@~a" filepath) clatter.core.constants:+crafterbin-url+)
           :output :string
           :error-output :string
           :ignore-error-status t)
        (declare (ignore error-output))
        (if (= exit-code 0)
            (let ((url (string-trim '(#\Space #\Newline #\Return) output)))
              (if (and (> (length url) 0) (search "https://" url))
                  (progn
                    ;; Copy to clipboard - try wl-copy first (Wayland), then X11 tools
                    (let ((copied (or (ignore-errors
                                        (zerop (nth-value 2 
                                          (uiop:run-program (list "wl-copy" url)
                                                            :output nil
                                                            :ignore-error-status t))))
                                      (ignore-errors
                                        (zerop (nth-value 2
                                          (uiop:run-program (list "xclip" "-selection" "clipboard")
                                                            :input (make-string-input-stream url)
                                                            :output nil
                                                            :ignore-error-status t))))
                                      (ignore-errors
                                        (zerop (nth-value 2
                                          (uiop:run-program (list "xsel" "--clipboard" "--input")
                                                            :input (make-string-input-stream url)
                                                            :output nil
                                                            :ignore-error-status t)))))))
                      (clatter.ui.tui:ui-submit
                        (clatter.core.dispatch:deliver-message
                         app buf
                         (clatter.core.model:make-message :level :system :nick "*crafterbin*"
                                                          :text (if copied
                                                                    (format nil "Uploaded: ~a (copied to clipboard)" url)
                                                                    (format nil "Uploaded: ~a" url)))))))
                  (clatter.ui.tui:ui-submit
                    (clatter.core.dispatch:deliver-message
                     app buf
                     (clatter.core.model:make-message :level :error :nick "*crafterbin*"
                                                      :text (format nil "Unexpected response: ~a" output))))))
            (clatter.ui.tui:ui-submit
              (clatter.core.dispatch:deliver-message
               app buf
               (clatter.core.model:make-message :level :error :nick "*crafterbin*"
                                                :text (format nil "Upload failed (exit ~a)" exit-code))))))
    (error (e)
      (clatter.ui.tui:ui-submit
        (clatter.core.dispatch:deliver-message
         app buf
         (clatter.core.model:make-message :level :error :nick "*crafterbin*"
                                          :text (format nil "Error: ~a" e)))))))

(defun handle-dcc-command (app conn args)
  "Handle /dcc commands.
   /dcc chat <nick> - initiate DCC chat
   /dcc send <nick> <file> - send a file
   /dcc list - list pending/active DCC connections
   /dcc accept [id] - accept pending DCC offer
   /dcc reject [id] - reject pending DCC offer
   /dcc close [id] - close DCC connection"
  (let ((manager clatter.net.dcc:*dcc-manager*))
    (unless manager
      (clatter.ui.tui:ui-submit
        (clatter.core.dispatch:deliver-message
         app (clatter.core.model:current-buffer app)
         (clatter.core.model:make-message :level :error :text "DCC not initialized")))
      (return-from handle-dcc-command))
    (multiple-value-bind (subcmd rest) (split-first-word args)
      (let ((subcmd-up (string-upcase subcmd)))
        (cond
          ;; /dcc chat <nick>
          ((string= subcmd-up "CHAT")
           (multiple-value-bind (nick rest2) (split-first-word rest)
             (declare (ignore rest2))
             (if (> (length nick) 0)
                 (clatter.net.dcc:dcc-initiate-chat manager nick)
                 (dcc-show-usage app "Usage: /dcc chat <nick>"))))
          
          ;; /dcc send <nick> <file>
          ((string= subcmd-up "SEND")
           (multiple-value-bind (nick filepath) (split-first-word rest)
             (if (and (> (length nick) 0) (> (length filepath) 0))
                 (clatter.net.dcc:dcc-initiate-send manager nick filepath conn)
                 (dcc-show-usage app "Usage: /dcc send <nick> <filepath>"))))
          
          ;; /dcc list
          ((string= subcmd-up "LIST")
           (dcc-show-list app manager))
          
          ;; /dcc accept [id]
          ((string= subcmd-up "ACCEPT")
           (dcc-accept-command app manager rest))
          
          ;; /dcc reject [id]
          ((string= subcmd-up "REJECT")
           (dcc-reject-command app manager rest))
          
          ;; /dcc close [id]
          ((string= subcmd-up "CLOSE")
           (dcc-close-command app manager rest))
          
          ;; /dcc ip [address] - show or set DCC IP
          ((string= subcmd-up "IP")
           (if (> (length rest) 0)
               (progn
                 (clatter.net.dcc:set-dcc-ip (string-trim " " rest))
                 (dcc-show-usage app (format nil "DCC IP set to: ~a" (string-trim " " rest))))
               (dcc-show-usage app (format nil "Current DCC IP: ~a" (clatter.net.dcc:get-local-ip)))))
          
          ;; Unknown or no subcommand - show help
          (t
           (dcc-show-usage app "Usage: /dcc [chat|send|list|accept|reject|close|ip] ...")))))))

(defun dcc-show-usage (app text)
  "Show DCC usage message."
  (clatter.ui.tui:ui-submit
    (clatter.core.dispatch:deliver-message
     app (clatter.core.model:current-buffer app)
     (clatter.core.model:make-message :level :system :text text))))

(defun dcc-show-list (app manager)
  "Show list of DCC connections."
  (let ((connections (clatter.net.dcc:dcc-manager-list manager)))
    (clatter.ui.tui:ui-submit
      (let ((buf (clatter.core.model:current-buffer app)))
        (if connections
            (progn
              (clatter.core.dispatch:deliver-message
               app buf
               (clatter.core.model:make-message :level :system :text "DCC connections:"))
              (dolist (conn connections)
                (clatter.core.dispatch:deliver-message
                 app buf
                 (clatter.core.model:make-message 
                  :level :system 
                  :text (format nil "  [~a] ~a" 
                                (clatter.net.dcc:dcc-id conn)
                                (clatter.net.dcc:dcc-status-string conn))))))
            (clatter.core.dispatch:deliver-message
             app buf
             (clatter.core.model:make-message :level :system :text "No DCC connections")))))))

(defun dcc-accept-command (app manager id-str)
  "Accept a pending DCC connection."
  (let* ((id (if (> (length id-str) 0)
                 (ignore-errors (parse-integer id-str))
                 ;; If no ID, accept first pending
                 (let ((pending (clatter.net.dcc:dcc-manager-pending manager)))
                   (when pending (clatter.net.dcc:dcc-id (first pending))))))
         (conn (when id (clatter.net.dcc:dcc-manager-find manager id))))
    (cond
      ((null conn)
       (dcc-show-usage app "No pending DCC connection to accept"))
      ((not (eq (clatter.net.dcc:dcc-state conn) :pending))
       (dcc-show-usage app (format nil "DCC ~a is not pending (state=~a)" 
                                   id (clatter.net.dcc:dcc-state conn))))
      (t
       (clatter.net.dcc:dcc-accept conn manager)))))

(defun dcc-reject-command (app manager id-str)
  "Reject a pending DCC connection."
  (let* ((id (if (> (length id-str) 0)
                 (ignore-errors (parse-integer id-str))
                 (let ((pending (clatter.net.dcc:dcc-manager-pending manager)))
                   (when pending (clatter.net.dcc:dcc-id (first pending))))))
         (conn (when id (clatter.net.dcc:dcc-manager-find manager id))))
    (cond
      ((null conn)
       (dcc-show-usage app "No pending DCC connection to reject"))
      ((not (eq (clatter.net.dcc:dcc-state conn) :pending))
       (dcc-show-usage app (format nil "DCC ~a is not pending" id)))
      (t
       (clatter.net.dcc:dcc-reject conn manager)))))

(defun dcc-close-command (app manager id-str)
  "Close a DCC connection."
  (let* ((id (when (> (length id-str) 0)
               (ignore-errors (parse-integer id-str))))
         (conn (when id (clatter.net.dcc:dcc-manager-find manager id))))
    (if conn
        (progn
          (clatter.net.dcc:dcc-close conn)
          (clatter.net.dcc:dcc-manager-remove manager id)
          (clatter.ui.tui:ui-submit
            (clatter.core.dispatch:deliver-message
             app (clatter.core.model:current-buffer app)
             (clatter.core.model:make-message :level :system 
                                              :text (format nil "DCC ~a closed" id)))))
        (dcc-show-usage app "Usage: /dcc close <id>"))))

(defun handle-input-line (app conn line)
  "Handle a line of input - either command or chat message.
   If conn is nil, try to get connection from active buffer."
  (let ((conn (or conn (clatter.core.model:get-current-connection app)))
        (parsed (parse-command line)))
    (if parsed
        ;; It's a command
        (let ((cmd (car parsed))
              (args (cdr parsed)))
          (unless (execute-command app conn cmd args)
            ;; Unknown command - show error
            (clatter.ui.tui:ui-submit
              (clatter.core.dispatch:deliver-message
               app (clatter.core.model:current-buffer app)
               (clatter.core.model:make-message :level :error :nick "*"
                                                :text (format nil "Unknown command: /~a" cmd))))))
        ;; It's a chat message - send to active buffer's target (respects split pane)
        (let* ((buf (clatter.core.model:active-buffer app))
               (target (if buf (clatter.core.model:buffer-title buf) ""))
               (kind (if buf (clatter.core.model:buffer-kind buf) :server)))
          (cond
            ;; Regular IRC channel/query
            ((and buf conn (member kind '(:channel :query)) (> (length target) 0))
             (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg target line))
             ;; Echo locally
             (clatter.ui.tui:ui-submit
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message :level :chat
                                                 :nick (clatter.net.irc:irc-nick conn)
                                                 :text line))))
            ;; DCC chat buffer
            ((and buf (eq kind :dcc-chat))
             (handle-dcc-chat-input app conn buf line)))))))

(defun handle-dcc-chat-input (app conn buf line)
  "Handle input in a DCC chat buffer."
  (handler-case
      (let* ((manager clatter.net.dcc:*dcc-manager*)
             (dcc-conn (when manager 
                         (clatter.net.dcc:find-dcc-connection-for-buffer manager buf))))
        (when dcc-conn
          (if (clatter.net.dcc:dcc-chat-send dcc-conn line)
              ;; Echo locally with our nick
              (let ((my-nick (if conn (clatter.net.irc:irc-nick conn) "me")))
                (clatter.ui.tui:ui-submit
                  (clatter.core.dispatch:deliver-message
                   app buf
                   (clatter.core.model:make-message :level :chat
                                                    :nick my-nick
                                                    :text line))))
              ;; Send failed
              (clatter.ui.tui:ui-submit
                (clatter.core.dispatch:deliver-message
                 app buf
                 (clatter.core.model:make-message :level :error
                                                  :text "DCC send failed - connection may be closed"))))))
    (error (e)
      (clatter.ui.tui:ui-submit
        (clatter.core.dispatch:deliver-message
         app buf
         (clatter.core.model:make-message :level :error
                                          :text (format nil "DCC error: ~a" e)))))))
