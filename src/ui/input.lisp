(in-package #:clatter.ui.input)

;; Track last typing notification time to avoid spamming
(defvar *last-typing-sent* 0)
;; Typing throttle uses clatter.core.constants:+typing-throttle-seconds+

(defun input-set-text (app new-text &optional (cursor (length new-text)))
  (setf (input-text (ui-input (app-ui app))) new-text)
  (setf (input-cursor (ui-input (app-ui app))) cursor)
  (mark-dirty app :input)
  app)

(defun %istate (app)
  (ui-input (app-ui app)))

(defun maybe-send-typing (app)
  "Send typing notification if enough time has passed since last one."
  (let ((now (get-universal-time))
        (conn (clatter.core.model:get-current-connection app)))
    (when (and conn (> (- now *last-typing-sent*) clatter.core.constants:+typing-throttle-seconds+))
      (let ((buf (clatter.core.model:active-buffer app)))
        (when (and buf (not (eq (clatter.core.model:buffer-kind buf) :server)))
          (setf *last-typing-sent* now)
          (clatter.net.irc:irc-send-typing conn (clatter.core.model:buffer-title buf) :active))))))

(defun input-insert-char (app ch)
  (let* ((st (%istate app))
         (s (input-text st))
         (i (input-cursor st)))
    (setf (input-text st) (concatenate 'string (subseq s 0 i) (string ch) (subseq s i)))
    (incf (input-cursor st))
    (mark-dirty app :input)
    ;; Send typing notification
    (maybe-send-typing app)))

(defun input-backspace (app)
  (let* ((st (%istate app))
         (s (input-text st))
         (i (input-cursor st)))
    (when (> i 0)
      (setf (input-text st) (concatenate 'string (subseq s 0 (1- i)) (subseq s i)))
      (decf (input-cursor st))
      (mark-dirty app :input))))

(defun input-delete (app)
  (let* ((st (%istate app))
         (s (input-text st))
         (i (input-cursor st)))
    (when (< i (length s))
      (setf (input-text st) (concatenate 'string (subseq s 0 i) (subseq s (1+ i))))
      (mark-dirty app :input))))

(defun input-move-left (app)
  (let ((st (%istate app)))
    (when (> (input-cursor st) 0)
      (decf (input-cursor st))
      (mark-dirty app :input))))

(defun input-move-right (app)
  (let* ((st (%istate app))
         (s (input-text st)))
    (when (< (input-cursor st) (length s))
      (incf (input-cursor st))
      (mark-dirty app :input))))

(defun input-move-home (app)
  (let ((st (%istate app)))
    (setf (input-cursor st) 0)
    (mark-dirty app :input)))

(defun input-move-end (app)
  (let* ((st (%istate app))
         (s (input-text st)))
    (setf (input-cursor st) (length s))
    (mark-dirty app :input)))

(defun input-history-prev (app)
  (let* ((st (%istate app))
         (hist (input-history st)))
    (when (> (fill-pointer hist) 0)
      (let ((pos (or (input-history-pos st) (fill-pointer hist))))
        (setf pos (max 0 (1- pos)))
        (setf (input-history-pos st) pos)
        (input-set-text app (aref hist pos))))))

(defun input-history-next (app)
  (let* ((st (%istate app))
         (hist (input-history st)))
    (when (> (fill-pointer hist) 0)
      (let ((pos (input-history-pos st)))
        (when pos
          (setf pos (min (fill-pointer hist) (1+ pos)))
          (if (= pos (fill-pointer hist))
              (progn
                (setf (input-history-pos st) nil)
                (input-set-text app ""))
              (progn
                (setf (input-history-pos st) pos)
                (input-set-text app (aref hist pos)))))))))

(defun input-submit-line (app)
  (let* ((st (%istate app))
         (line (string-trim '(#\Space #\Tab) (input-text st))))
    (when (> (length line) 0)
      ;; push into history
      (let ((hist (input-history st)))
        (vector-push-extend line hist)
        (setf (input-history-pos st) nil))
      ;; Get connection for current buffer
      (let ((conn (clatter.core.model:get-current-connection app)))
        (when conn
          (let ((buf (clatter.core.model:active-buffer app)))
            (when (and buf (not (eq (clatter.core.model:buffer-kind buf) :server)))
              (clatter.net.irc:irc-send-typing conn (clatter.core.model:buffer-title buf) :done))))
        ;; Handle via command system
        (clatter.core.commands:handle-input-line app conn line)))
    (input-set-text app "" 0)))

(defun get-command-completions (prefix)
  "Get list of commands matching PREFIX (without leading /)."
  (let ((matches nil))
    (maphash (lambda (name class-sym)
               (declare (ignore class-sym))
               (when (and (>= (length name) (length prefix))
                          (string-equal prefix (subseq name 0 (length prefix))))
                 (push (string-downcase name) matches)))
             clatter.core.commands::*command-registry*)
    (sort (remove-duplicates matches :test #'string-equal) #'string-lessp)))

(defun get-channel-completions (app prefix)
  "Get list of channels matching PREFIX from current buffers."
  (let ((matches nil))
    (loop for buf across (clatter.core.model:app-buffers app)
          when (and buf 
                    (eq (clatter.core.model:buffer-kind buf) :channel)
                    (let ((title (clatter.core.model:buffer-title buf)))
                      (and (>= (length title) (length prefix))
                           (string-equal prefix (subseq title 0 (length prefix))))))
          do (push (clatter.core.model:buffer-title buf) matches))
    (sort matches #'string-lessp)))

(defun get-nick-completions (buf prefix)
  "Get list of nicks matching PREFIX from buffer members."
  (let ((matches nil)
        (members (when buf (clatter.core.model:buffer-members buf))))
    (when members
      (maphash (lambda (nick val)
                 (declare (ignore val))
                 (when (and (>= (length nick) (length prefix))
                            (string-equal prefix (subseq nick 0 (length prefix))))
                   (push nick matches)))
               members))
    (sort matches #'string-lessp)))

(defun input-tab-complete (app)
  "Tab-complete at cursor position.
   Completes: commands (after /), channels (after # or in /join), nicks."
  (let* ((st (%istate app))
         (text (input-text st))
         (cursor (input-cursor st))
         (buf (active-buffer app)))  ;; Use active-buffer for split pane support
    ;; Find word start (go back to space or start of line)
    (let* ((word-start (or (position #\Space text :end cursor :from-end t) -1))
           (word-start (1+ word-start))
           (prefix (subseq text word-start cursor)))
      (when (> (length prefix) 0)
        (let ((matches nil)
              (suffix " "))
          ;; Determine completion type
          (cond
            ;; Command completion: starts with / at beginning of line
            ((and (= word-start 0) (char= (char prefix 0) #\/))
             (let ((cmd-prefix (subseq prefix 1)))
               (setf matches (mapcar (lambda (m) (concatenate 'string "/" m))
                                     (get-command-completions cmd-prefix)))))
            ;; Channel completion: starts with #
            ((char= (char prefix 0) #\#)
             (setf matches (get-channel-completions app prefix)))
            ;; Nick completion (default)
            (t
             (setf matches (get-nick-completions buf prefix))
             ;; Add ": " suffix if at start of line (addressing someone)
             (when (= word-start 0)
               (setf suffix ": "))))
          ;; Apply first match
          (when matches
            (let* ((completion (first matches))
                   (new-text (concatenate 'string
                                          (subseq text 0 word-start)
                                          completion
                                          suffix
                                          (subseq text cursor))))
              (input-set-text app new-text (+ word-start (length completion) (length suffix))))))))))
