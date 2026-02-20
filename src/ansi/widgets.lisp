(in-package #:clatter.ui.widgets)

;;; Widget System for CLatter UI - CLOS-based
;;; Panels and widgets for the TUI

;;; ============================================================
;;; URL Detection and Clickable Links
;;; ============================================================

(defparameter *url-regex* 
  (cl-ppcre:create-scanner "https?://[^\\s<>\"']+")
  "Regex to match URLs in text")

(defun print-text-with-links (text)
  "Print TEXT with URLs rendered as clickable hyperlinks."
  (let ((pos 0)
        (len (length text)))
    (cl-ppcre:do-matches (start end *url-regex* text)
      ;; Print text before URL
      (when (> start pos)
        (princ (subseq text pos start) *terminal-io*))
      ;; Print URL as hyperlink
      (let ((url (subseq text start end)))
        (clatter.ansi:begin-hyperlink url)
        (clatter.ansi:underline)
        (princ url *terminal-io*)
        (clatter.ansi:reset))
      (setf pos end))
    ;; Print remaining text after last URL
    (when (< pos len)
      (princ (subseq text pos) *terminal-io*))))

;;; ============================================================
;;; Base Panel Class
;;; ============================================================

(defclass panel ()
  ((x :initarg :x :accessor panel-x :initform 1)
   (y :initarg :y :accessor panel-y :initform 1)
   (width :initarg :width :accessor panel-width :initform 10)
   (height :initarg :height :accessor panel-height :initform 10)
   (visible-p :initarg :visible :accessor panel-visible-p :initform t)
   (border-p :initarg :border :accessor panel-border-p :initform t)
   (title :initarg :title :accessor panel-title :initform nil)
   (active-p :initarg :active :accessor panel-active-p :initform nil))
  (:documentation "Base class for UI panels"))

(defgeneric panel-render (panel)
  (:documentation "Render the panel to the terminal"))

(defgeneric panel-clear (panel)
  (:documentation "Clear the panel area"))

(defgeneric panel-content-x (panel)
  (:documentation "X coordinate of content area"))

(defgeneric panel-content-y (panel)
  (:documentation "Y coordinate of content area"))

(defgeneric panel-content-width (panel)
  (:documentation "Width of content area"))

(defgeneric panel-content-height (panel)
  (:documentation "Height of content area"))

(defmethod panel-content-x ((panel panel))
  (if (panel-border-p panel) (1+ (panel-x panel)) (panel-x panel)))

(defmethod panel-content-y ((panel panel))
  (if (panel-border-p panel) (1+ (panel-y panel)) (panel-y panel)))

(defmethod panel-content-width ((panel panel))
  (if (panel-border-p panel) (- (panel-width panel) 2) (panel-width panel)))

(defmethod panel-content-height ((panel panel))
  (if (panel-border-p panel) (- (panel-height panel) 2) (panel-height panel)))

(defmethod panel-clear ((panel panel))
  "Clear only the content area of the panel (not the border).
   Note: To reduce flicker, prefer padding lines to full width instead of clearing."
  (when (panel-visible-p panel)
    ;; Don't set background color - use terminal default to avoid dark areas
    (clatter.ansi:fill-rect (panel-content-x panel) (panel-content-y panel)
                             (panel-content-width panel) (panel-content-height panel))
    (clatter.ansi:reset)))

(defmethod panel-render :before ((panel panel))
  (when (panel-visible-p panel)
    ;; Draw border only (no clearing - content will overwrite with padded lines)
    (when (panel-border-p panel)
      (let* ((theme (clatter.ui.theme:current-theme))
             (border-color (if (panel-active-p panel)
                               (clatter.ui.theme:theme-border-active theme)
                               (clatter.ui.theme:theme-border-inactive theme))))
        (when border-color
          (clatter.ansi:emit-fg border-color *terminal-io*))
        (clatter.ansi:draw-box (panel-x panel) (panel-y panel)
                                (panel-width panel) (panel-height panel)
                                :tl (clatter.ui.theme:theme-box-tl theme)
                                :tr (clatter.ui.theme:theme-box-tr theme)
                                :bl (clatter.ui.theme:theme-box-bl theme)
                                :br (clatter.ui.theme:theme-box-br theme)
                                :h (clatter.ui.theme:theme-box-h theme)
                                :v (clatter.ui.theme:theme-box-v theme))
        ;; Draw title if present
        (when (panel-title panel)
          (clatter.ansi:cursor-to (panel-y panel) (+ (panel-x panel) 2))
          (princ (panel-title panel) *terminal-io*))
        (clatter.ansi:reset)))))

(defmethod panel-render ((panel panel))
  ;; Base implementation does nothing beyond border
  nil)

;;; ============================================================
;;; Buffer List Panel
;;; ============================================================

(defclass buflist-panel (panel)
  ((app :initarg :app :accessor buflist-app))
  (:default-initargs :title " buffers " :border t))

(defun get-active-buffer-id (app)
  "Get the buffer ID for the currently active pane.
   In split mode with secondary pane active, returns split-buffer-id.
   Otherwise returns current-buffer-id."
  (let* ((ui (clatter.core.model:app-ui app))
         (split-mode (clatter.core.model:ui-split-mode ui))
         (active-pane (clatter.core.model:ui-active-pane ui)))
    (if (and split-mode
             (or (and (eq split-mode :horizontal) (eq active-pane :right))
                 (and (eq split-mode :vertical) (eq active-pane :bottom))))
        (clatter.core.model:ui-split-buffer-id ui)
        (clatter.core.model:app-current-buffer-id app))))

(defmethod panel-render ((panel buflist-panel))
  (when (panel-visible-p panel)
    ;; Ensure clean state before rendering content
    (clatter.ansi:reset)
    (let* ((app (buflist-app panel))
           (theme (clatter.ui.theme:current-theme))
           (content-x (panel-content-x panel))
           (content-y (panel-content-y panel))
           (content-w (panel-content-width panel))
           (content-h (panel-content-height panel))
           (buffers (clatter.core.model:app-buffers app))
           (current-id (get-active-buffer-id app))  ;; Use active pane's buffer
           (row 0)
           (visual-order nil)
           (networks (make-hash-table :test 'equal))
           (network-order nil))
      ;; Group buffers by network
      (loop for i from 0 below (length buffers)
            for buf = (aref buffers i)
            when buf
            do (let ((net (or (clatter.core.model:buffer-network buf) "unknown")))
                 (unless (gethash net networks)
                   (setf (gethash net networks) nil)
                   (push (cons net i) network-order))
                 (push (cons i buf) (gethash net networks))))
      ;; Sort networks by first appearance
      (setf network-order (sort network-order #'< :key #'cdr))
      ;; Render each network's buffers
      (dolist (net-pair network-order)
        (let* ((net-name (car net-pair))
               (buf-list (gethash net-name networks))
               (sorted (sort (copy-list buf-list)
                             (lambda (a b)
                               (let ((ka (clatter.core.model:buffer-kind (cdr a)))
                                     (kb (clatter.core.model:buffer-kind (cdr b))))
                                 (cond ((eq ka :server) t)
                                       ((eq kb :server) nil)
                                       (t (string< (clatter.core.model:buffer-title (cdr a))
                                                   (clatter.core.model:buffer-title (cdr b))))))))))
          (dolist (pair sorted)
            (push (car pair) visual-order)
            (when (< row content-h)
              (let* ((i (car pair))
                     (buf (cdr pair))
                     (current-p (= i current-id))
                     (indent (if (typep buf 'clatter.core.model:server-buffer) "" "  "))
                     (unread (clatter.core.model:buffer-unread-count buf))
                     (highlights (clatter.core.model:buffer-highlight-count buf))
                     (title (clatter.core.model:buffer-title buf))
                     (indicator (cond
                                  ((> highlights 0) 
                                   (format nil "~a(~d)" indent highlights))
                                  ((> unread 0)
                                   (format nil "~a[~d]" indent unread))
                                  (t indent)))
                     (line (format nil "~a~a" indicator title)))
                ;; Position and draw - reset before each line to clear any stale state
                (clatter.ansi:reset)
                (clatter.ansi:cursor-to (+ content-y row) content-x)
                ;; Truncate or pad line to exactly content-w
                (let* ((display-line (subseq line 0 (min (length line) content-w)))
                       (padded-line (format nil "~va" content-w display-line)))
                  ;; Style based on state
                  (cond
                    (current-p
                     (clatter.ansi:inverse)
                     (princ padded-line *terminal-io*)
                     (clatter.ansi:reset))
                    ((> highlights 0)
                     (clatter.ansi:emit-fg (clatter.ui.theme:theme-mention-indicator theme) *terminal-io*)
                     (clatter.ansi:bold)
                     (princ padded-line *terminal-io*)
                     (clatter.ansi:reset))
                    ((> unread 0)
                     (clatter.ansi:emit-fg (clatter.ui.theme:theme-unread-indicator theme) *terminal-io*)
                     (princ padded-line *terminal-io*)
                     (clatter.ansi:reset))
                    (t
                     (princ padded-line *terminal-io*)
                     (clatter.ansi:reset))))
                (incf row))))))
      ;; Store visual order (always update to keep in sync)
      (setf (clatter.core.model:app-buffer-order app) (nreverse visual-order)))))

;;; ============================================================
;;; Chat Panel
;;; ============================================================

(defclass chat-panel (panel)
  ((buffer :initarg :buffer :accessor chat-buffer :initform nil)
   (time-format :initarg :time-format :accessor chat-time-format :initform "%H:%M"))
  (:default-initargs :border t))

(defun format-time (universal-time fmt)
  "Format universal time using format string."
  (multiple-value-bind (sec min hour) (decode-universal-time universal-time)
    (let* ((hour12 (let ((h (mod hour 12))) (if (zerop h) 12 h)))
           (ampm (if (< hour 12) "AM" "PM"))
           (result fmt))
      (setf result (cl-ppcre:regex-replace-all "%H" result (format nil "~2,'0d" hour)))
      (setf result (cl-ppcre:regex-replace-all "%I" result (format nil "~2,'0d" hour12)))
      (setf result (cl-ppcre:regex-replace-all "%M" result (format nil "~2,'0d" min)))
      (setf result (cl-ppcre:regex-replace-all "%S" result (format nil "~2,'0d" sec)))
      (setf result (cl-ppcre:regex-replace-all "%p" result ampm))
      result)))

(defun wrap-text (text width)
  "Wrap text to fit within width, returning list of lines."
  (if (<= (length text) width)
      (list text)
      (let ((lines nil)
            (start 0))
        (loop while (< start (length text))
              for end = (min (+ start width) (length text))
              for break-pos = (if (< end (length text))
                                  (or (position #\Space text :start start :end end :from-end t)
                                      end)
                                  end)
              for actual-end = (if (= break-pos start) end break-pos)
              do (push (subseq text start actual-end) lines)
                 (setf start (if (and (< actual-end (length text))
                                      (char= (char text actual-end) #\Space))
                                 (1+ actual-end)
                                 actual-end)))
        (nreverse lines))))

(defmethod panel-render ((panel chat-panel))
  (when (and (panel-visible-p panel) (chat-buffer panel))
    (let* ((buf (chat-buffer panel))
           (theme (clatter.ui.theme:current-theme))
           (content-x (panel-content-x panel))
           (content-y (panel-content-y panel))
           (content-w (panel-content-width panel))
           (content-h (panel-content-height panel))
           (all-msgs (clatter.core.ring:ring->list (clatter.core.model:buffer-scrollback buf)))
           ;; Apply filter if active
           (msgs (if (clatter.core.model:buffer-filter-active buf)
                     (let ((pattern (clatter.core.model:buffer-filter-pattern buf)))
                       (remove-if-not 
                        (lambda (m)
                          (search pattern (clatter.core.model:message-text m) :test #'char-equal))
                        all-msgs))
                     all-msgs))
           (offset (clatter.core.model:buffer-scroll-offset buf))
           (time-fmt (chat-time-format panel)))
      ;; Build display lines
      (let ((message-groups nil))
        (loop for m in msgs
              for ts = (clatter.core.model:message-ts m)
              for time-str = (format-time ts time-fmt)
              for nick = (or (clatter.core.model:message-nick m) "*")
              for text = (clatter.core.model:message-text m)
              for level = (clatter.core.model:message-level m)
              for highlightp = (clatter.core.model:message-highlight m)
              for nick-display = (format nil "[~a] ~a: " time-str nick)
              for nick-len = (length nick-display)
              for text-width = (max 1 (- content-w nick-len))
              for wrapped = (wrap-text text text-width)
              do (let ((msg-lines nil)
                       (first-line t))
                   (dolist (line wrapped)
                     (push (list :nick (if first-line nick-display
                                           (make-string nick-len :initial-element #\Space))
                                 :text line
                                 :highlight highlightp
                                 :nick-raw nick
                                 :level level
                                 :first first-line)
                           msg-lines)
                     (setf first-line nil))
                   (push (nreverse msg-lines) message-groups)))
        ;; Display from top - newest messages at top
        (let* ((display-lines (apply #'append message-groups))
               (total (length display-lines))
               (start (min offset (max 0 (- total content-h))))
               (visible (subseq display-lines start (min (+ start content-h) total)))
               (y 0))
          (dolist (dl visible)
            (when (< y content-h)
              (let* ((nick-display (getf dl :nick))
                     (text-display (getf dl :text))
                     (highlightp (getf dl :highlight))
                     (nick-raw (getf dl :nick-raw))
                     (level (getf dl :level))
                     (firstp (getf dl :first))
                     (lvl-color (clatter.ui.theme:theme-level-color theme level)))
                (clatter.ansi:cursor-to (+ content-y y) content-x)
                ;; Build full line and pad to content width to avoid clearing
                (let* ((full-line (concatenate 'string nick-display text-display))
                       (line-len (length full-line))
                       (padding (max 0 (- content-w line-len)))
                       (pad-str (make-string padding :initial-element #\Space)))
                  (cond
                    ;; Highlighted messages
                    (highlightp
                     (clatter.ansi:emit-fg (clatter.ui.theme:theme-mention-indicator theme) *terminal-io*)
                     (clatter.ansi:bold)
                     (princ full-line *terminal-io*)
                     (clatter.ansi:reset)
                     (princ pad-str *terminal-io*))
                    ;; Level-colored messages
                    (lvl-color
                     (clatter.ansi:emit-fg lvl-color *terminal-io*)
                     (princ full-line *terminal-io*)
                     (clatter.ansi:reset)
                     (princ pad-str *terminal-io*))
                    ;; Regular messages - render URLs as clickable links
                    (t
                     (when firstp
                       (let ((nick-color (clatter.ui.theme:theme-nick-color theme nick-raw)))
                         (when nick-color
                           (clatter.ansi:emit-fg nick-color *terminal-io*))))
                     (princ nick-display *terminal-io*)
                     (clatter.ansi:reset)
                     (print-text-with-links text-display)
                     (princ pad-str *terminal-io*))))
              (incf y)))
          ;; Clear any remaining lines below messages
          (let ((blank-line (make-string content-w :initial-element #\Space)))
            (loop while (< y content-h) do
              (clatter.ansi:cursor-to (+ content-y y) content-x)
              (princ blank-line *terminal-io*)
              (incf y)))))))))

;;; ============================================================
;;; Nick List Panel
;;; ============================================================

(defclass nicklist-panel (panel)
  ((buffer :initarg :buffer :accessor nicklist-buffer :initform nil))
  (:default-initargs :title " nicks " :border t))

(defmethod panel-render ((panel nicklist-panel))
  (when (and (panel-visible-p panel) (nicklist-buffer panel))
    (let* ((buf (nicklist-buffer panel))
           (content-x (panel-content-x panel))
           (content-y (panel-content-y panel))
           (content-w (panel-content-width panel))
           (content-h (panel-content-height panel))
           (row 0))
      (when (clatter.core.model:buffer-members buf)
        (let ((nick-list nil))
          (maphash (lambda (nick val)
                     (declare (ignore val))
                     (push nick nick-list))
                   (clatter.core.model:buffer-members buf))
          (setf nick-list (sort nick-list #'string-lessp))
          (dolist (nick nick-list)
            (when (< row content-h)
              (clatter.ansi:cursor-to (+ content-y row) content-x)
              (princ (subseq nick 0 (min (length nick) content-w)) *terminal-io*)
              (incf row))))))))

;;; ============================================================
;;; Status Bar Panel
;;; ============================================================

(defclass status-panel (panel)
  ((app :initarg :app :accessor status-app)
   (buffer :initarg :buffer :accessor status-buffer :initform nil))
  (:default-initargs :border nil :height 1))

(defmethod panel-render ((panel status-panel))
  (when (panel-visible-p panel)
    (let* ((app (status-app panel))
           (buf (status-buffer panel))
           (theme (clatter.ui.theme:current-theme))
           (x (panel-x panel))
           (y (panel-y panel))
           (w (panel-width panel)))
      ;; Clear line
      (clatter.ansi:cursor-to y x)
      (clatter.ansi:clear-to-eol)
      (when buf
        (let* ((conn (clatter.core.model:get-buffer-connection app buf))
               (conn-state (when conn (clatter.net.irc:irc-state conn)))
               (conn-indicator (case conn-state
                                 (:connected (clatter.ui.theme:theme-connected-indicator theme))
                                 (:connecting (clatter.ui.theme:theme-connecting-indicator theme))
                                 (:registering "⋯")
                                 (:disconnected (clatter.ui.theme:theme-disconnected-indicator theme))
                                 (t "?")))
               (tls-indicator (when (and conn (clatter.net.irc:irc-network-config conn))
                                (if (clatter.core.config:network-config-tls 
                                     (clatter.net.irc:irc-network-config conn))
                                    (clatter.ui.theme:theme-tls-indicator theme) nil)))
               (filter-indicator (when (clatter.core.model:buffer-filter-active buf)
                                   (format nil "~a~a" 
                                           (clatter.ui.theme:theme-filter-indicator theme)
                                           (clatter.core.model:buffer-filter-pattern buf))))
               (scroll-indicator (when (> (clatter.core.model:buffer-scroll-offset buf) 0)
                                   (format nil "~a~d" 
                                           (clatter.ui.theme:theme-scroll-indicator theme)
                                           (clatter.core.model:buffer-scroll-offset buf))))
               (unread (clatter.core.model:buffer-unread-count buf))
               (highlights (clatter.core.model:buffer-highlight-count buf))
               (typing-nicks (clatter.core.model:get-typing-nicks buf))
               (channel-modes (clatter.core.model:buffer-channel-modes buf))
               (my-modes (clatter.core.model:buffer-my-modes buf))
               (typing-str (when typing-nicks
                             (if (= (length typing-nicks) 1)
                                 (format nil "~a is typing..." (first typing-nicks))
                                 (format nil "~{~a~^, ~} are typing..." typing-nicks))))
               (title-str (if (and channel-modes (> (length channel-modes) 0))
                              (format nil "~a [~a]" (clatter.core.model:buffer-title buf) channel-modes)
                              (clatter.core.model:buffer-title buf)))
               (network-name (clatter.core.model:buffer-network buf))
               (mode-str (when (and my-modes (> (length my-modes) 0))
                           (format nil "(~a)" my-modes)))
               (shortcuts " | ^Q quit | ^L redraw | ^P/N buf | ^U/D scroll | ^W split | ^T orient | ^X pane | ^K nicks")
               (line (format nil " ~a~@[~a~] [~a]~@[ ~a~]~@[  ~d unread~]~@[  ~d mentions~]~@[  ~a~]~@[  ~a~]~@[  ~a~]~a"
                             conn-indicator
                             tls-indicator
                             (if network-name (format nil "~a/~a" network-name title-str) title-str)
                             mode-str
                             (and (> unread 0) unread)
                             (and (> highlights 0) highlights)
                             scroll-indicator
                             filter-indicator
                             typing-str
                             shortcuts)))
          (clatter.ansi:cursor-to y x)
          (clatter.ansi:inverse)
          (princ (subseq line 0 (min (length line) w)) *terminal-io*)
          ;; Pad to full width
          (let ((remaining (- w (min (length line) w))))
            (when (> remaining 0)
              (princ (make-string remaining :initial-element #\Space) *terminal-io*)))
          (clatter.ansi:reset))))))

;;; ============================================================
;;; Input Panel
;;; ============================================================

(defclass input-panel (panel)
  ((input-state :initarg :input-state :accessor input-panel-state :initform nil))
  (:default-initargs :border nil :height 1))

(defmethod panel-render ((panel input-panel))
  (when (panel-visible-p panel)
    (let* ((theme (clatter.ui.theme:current-theme))
           (st (input-panel-state panel))
           (x (panel-x panel))
           (y (panel-y panel))
           (w (panel-width panel))
           (prompt (clatter.ui.theme:theme-input-prompt theme))
           (txt (if st (clatter.core.model:input-text st) ""))
           (cursor (if st (clatter.core.model:input-cursor st) 0))
           (prompt-len (length prompt))
           (visible-w (- w prompt-len))
           (margin 5)
           (scroll-offset (cond
                            ((< cursor visible-w) 0)
                            (t (- cursor (- visible-w margin)))))
           (visible-end (min (length txt) (+ scroll-offset visible-w)))
           (visible-txt (subseq txt scroll-offset visible-end))
           (display-prompt (if (> scroll-offset 0) "…" prompt))
           (full (concatenate 'string display-prompt visible-txt)))
      ;; Clear line
      (clatter.ansi:cursor-to y x)
      (clatter.ansi:clear-to-eol)
      ;; Draw input
      (clatter.ansi:cursor-to y x)
      (princ (subseq full 0 (min (length full) w)) *terminal-io*)
      ;; Position cursor
      (clatter.ansi:cursor-to y (+ x (length display-prompt) (- cursor scroll-offset)))
      (clatter.ansi:cursor-show)
      (force-output *terminal-io*))))
