(in-package #:clatter.ui.tui)

;;; TUI Main Loop - ANSI-based
;;; Replaces ncurses/croatoan with pure ANSI terminal control
;;; Note: ui-submit macro and event queue are defined in submit.lisp

;;; ============================================================
;;; Global State
;;; ============================================================

(defvar *layout* nil "Current layout instance")
(defvar *running* nil "Main loop running flag")

;;; ============================================================
;;; Event Processing
;;; ============================================================

(defun process-events ()
  "Process all pending events in the queue."
  (let ((events nil))
    (bordeaux-threads:with-lock-held (*event-lock*)
      (setf events (nreverse *event-queue*)
            *event-queue* nil))
    (dolist (thunk events)
      (handler-case
          (funcall thunk)
        (error (e)
          (clatter.core.debug:log-error "Event error: ~A" e))))))

;;; ============================================================
;;; Health Check
;;; ============================================================

(defvar *last-health-check* 0)

(defun maybe-check-connection-health (app)
  "Periodically check all connection health."
  (let ((now (get-universal-time)))
    (when (> (- now *last-health-check*) clatter.core.constants:+health-check-interval+)
      (setf *last-health-check* now)
      (maphash (lambda (name conn)
                 (declare (ignore name))
                 (when conn
                   (clatter.net.irc:irc-check-health conn)))
               (clatter.core.model:app-connections app)))))

;;; ============================================================
;;; Rendering
;;; ============================================================

(defun render-frame (app)
  "Render the entire UI frame."
  (maybe-check-connection-health app)
  
  (unless (clatter.core.model:dirty-p app)
    (return-from render-frame))
  
  ;; Update layout with current terminal size
  (let ((size (clatter.terminal:terminal-size)))
    (clatter.ui.layout:layout-compute *layout* (first size) (second size) app))
  
  ;; Render all panels
  (clatter.ui.layout:layout-render *layout*)
  
  (clatter.core.model:clear-dirty app))

;;; ============================================================
;;; Key Bindings
;;; ============================================================

(defun handle-key (app key)
  "Handle a key event and return t if handled."
  (let* ((code (clatter.terminal:key-event-code key))
         (char (clatter.terminal:key-event-char key))
         (ctrl-p (clatter.terminal:key-event-ctrl-p key))
         (alt-p (clatter.terminal:key-event-alt-p key))
         (ui (clatter.core.model:app-ui app))
         (input-st (clatter.core.model:ui-input ui)))
    (cond
      ;; Ctrl+Q - quit
      ((and ctrl-p (eql char #\q))
       (setf (clatter.core.model:app-quit-requested app) t)
       t)
      
      ;; Ctrl+P - previous buffer
      ((and ctrl-p (eql char #\p))
       (switch-buffer app -1)
       t)
      
      ;; Ctrl+N - next buffer
      ((and ctrl-p (eql char #\n))
       (switch-buffer app 1)
       t)
      
      ;; Ctrl+U - scroll up
      ((and ctrl-p (eql char #\u))
       (scroll-buffer app 10)
       t)
      
      ;; Ctrl+D - scroll down
      ((and ctrl-p (eql char #\d))
       (scroll-buffer app -10)
       t)
      
      ;; Ctrl+W - toggle split mode
      ((and ctrl-p (eql char #\w))
       (toggle-split app)
       t)
      
      ;; Ctrl+T - toggle split orientation (horizontal/vertical)
      ((and ctrl-p (eql char #\t))
       (toggle-split-orientation app)
       t)
      
      ;; Ctrl+X - switch active pane in split mode
      ((and ctrl-p (eql char #\x))
       (switch-active-pane app)
       t)
      
      ;; Ctrl+L - redraw screen
      ((and ctrl-p (eql char #\l))
       (redraw-screen app)
       t)
      
      ;; Ctrl+K - toggle nick list
      ((and ctrl-p (eql char #\k))
       (toggle-nicklist app)
       t)
      
      ;; Page Up
      ((eq code :page-up)
       (scroll-buffer app 20)
       t)
      
      ;; Page Down
      ((eq code :page-down)
       (scroll-buffer app -20)
       t)
      
      ;; Up arrow - history previous
      ((eq code :up)
       (clatter.ui.input:input-history-prev app)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; Down arrow - history next
      ((eq code :down)
       (clatter.ui.input:input-history-next app)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; Left arrow - cursor left
      ((eq code :left)
       (clatter.ui.input:input-move-left app)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; Right arrow - cursor right
      ((eq code :right)
       (clatter.ui.input:input-move-right app)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; Home - cursor to start
      ((eq code :home)
       (clatter.ui.input:input-move-home app)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; End - cursor to end
      ((eq code :end)
       (clatter.ui.input:input-move-end app)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; Backspace
      ((eq code :backspace)
       (clatter.ui.input:input-backspace app)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; Delete
      ((eq code :delete)
       (clatter.ui.input:input-delete app)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; Tab - completion
      ((eq code :tab)
       (clatter.ui.input:input-tab-complete app)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; Enter - submit
      ((eq code :enter)
       (clatter.ui.input:input-submit-line app)
       (clatter.core.model:mark-dirty app :input :chat)
       t)
      
      ;; Alt+number - switch to buffer N
      ((and alt-p char (digit-char-p char))
       (let ((n (digit-char-p char)))
         (when (and (> n 0) (<= n (length (clatter.core.model:app-buffers app))))
           (setf (clatter.core.model:app-current-buffer-id app) (1- n))
           (clatter.core.model:mark-dirty app :buflist :chat :status)))
       t)
      
      ;; Regular character
      ((and char (graphic-char-p char))
       (clatter.ui.input:input-insert-char app char)
       (clatter.core.model:mark-dirty app :input)
       t)
      
      ;; Unhandled
      (t nil))))

;;; ============================================================
;;; Buffer Navigation
;;; ============================================================

(defun switch-buffer (app direction)
  "Switch to next/previous buffer in visual order."
  (let* ((order (clatter.core.model:app-buffer-order app))
         (current-id (clatter.core.model:app-current-buffer-id app))
         (pos (position current-id order)))
    (when (and order pos)
      (let* ((new-pos (mod (+ pos direction) (length order)))
             (new-id (nth new-pos order)))
        (setf (clatter.core.model:app-current-buffer-id app) new-id)
        ;; Clear unread when switching to buffer
        (let ((buf (clatter.core.model:find-buffer app new-id)))
          (when buf
            (setf (clatter.core.model:buffer-unread-count buf) 0
                  (clatter.core.model:buffer-highlight-count buf) 0)))
        (clatter.core.model:mark-dirty app :buflist :chat :status)))))

(defun scroll-buffer (app amount)
  "Scroll the active buffer by amount lines."
  (let ((buf (clatter.core.model:active-buffer app)))
    (when buf
      (let* ((current (clatter.core.model:buffer-scroll-offset buf))
             (new-offset (max 0 (+ current amount))))
        (setf (clatter.core.model:buffer-scroll-offset buf) new-offset)
        (clatter.core.model:mark-dirty app :chat :status)))))

(defun toggle-split (app)
  "Toggle split pane mode."
  (let ((ui (clatter.core.model:app-ui app)))
    (cond
      ((null (clatter.core.model:ui-split-mode ui))
       ;; Enable horizontal split with next buffer
       (let* ((order (clatter.core.model:app-buffer-order app))
              (current-id (clatter.core.model:app-current-buffer-id app))
              (pos (position current-id order))
              (next-pos (when pos (mod (1+ pos) (length order))))
              (next-id (when next-pos (nth next-pos order))))
         (when (and next-id (/= next-id current-id))
           (setf (clatter.core.model:ui-split-mode ui) :horizontal
                 (clatter.core.model:ui-split-buffer-id ui) next-id
                 (clatter.core.model:ui-active-pane ui) :left)
           ;; Update layout's split mode
           (setf (clatter.ui.layout:layout-split-mode *layout*) :horizontal))))
      ((eq (clatter.core.model:ui-split-mode ui) :horizontal)
       ;; Switch to vertical split
       (setf (clatter.core.model:ui-split-mode ui) :vertical)
       (setf (clatter.ui.layout:layout-split-mode *layout*) :vertical))
      (t
       ;; Disable split
       (setf (clatter.core.model:ui-split-mode ui) nil
             (clatter.core.model:ui-split-buffer-id ui) nil)
       (setf (clatter.ui.layout:layout-split-mode *layout*) nil)))
    (clatter.core.model:mark-dirty app :layout :chat :status)))

(defun toggle-nicklist (app)
  "Toggle nick list visibility."
  (let ((ui (clatter.core.model:app-ui app)))
    (setf (clatter.core.model:ui-nicklist-visible ui)
          (not (clatter.core.model:ui-nicklist-visible ui)))
    (setf (clatter.ui.layout:layout-nicklist-visible *layout*)
          (clatter.core.model:ui-nicklist-visible ui))
    (clatter.core.model:mark-dirty app :layout :chat)))

(defun redraw-screen (app)
  "Force a complete screen redraw."
  (clatter.ansi:clear-screen)
  (clatter.core.model:mark-dirty app :layout :chat :buflist :status :input))

(defun toggle-split-orientation (app)
  "Toggle between horizontal and vertical split."
  (let ((ui (clatter.core.model:app-ui app)))
    (when (clatter.core.model:ui-split-mode ui)
      (let ((new-mode (if (eq (clatter.core.model:ui-split-mode ui) :horizontal)
                          :vertical
                          :horizontal)))
        (setf (clatter.core.model:ui-split-mode ui) new-mode)
        (setf (clatter.ui.layout:layout-split-mode *layout*) new-mode))
      (clatter.core.model:mark-dirty app :layout :chat :status))))

(defun switch-active-pane (app)
  "Switch which pane is active in split mode."
  (let ((ui (clatter.core.model:app-ui app)))
    (when (clatter.core.model:ui-split-mode ui)
      (setf (clatter.core.model:ui-active-pane ui)
            (if (member (clatter.core.model:ui-active-pane ui) '(:left :top))
                :right
                :left))
      (clatter.core.model:mark-dirty app :chat :status))))

;;; ============================================================
;;; Main TUI Loop
;;; ============================================================

(defun run-tui (app)
  "Run the main TUI event loop."
  (setf *running* t
        *event-queue* nil
        *layout* (clatter.ui.layout:make-layout
                  :buflist-width (clatter.core.model:ui-buflist-w 
                                  (clatter.core.model:app-ui app))
                  :nicklist-width (clatter.core.model:ui-nicklist-w
                                   (clatter.core.model:app-ui app))
                  :nicklist-visible (clatter.core.model:ui-nicklist-visible
                                     (clatter.core.model:app-ui app))))
  
  (clatter.terminal:with-raw-terminal
    ;; Initial render
    (clatter.ansi:clear-screen)
    (clatter.core.model:mark-dirty app :layout :chat :buflist :status :input)
    (render-frame app)
    
    ;; Main loop
    (loop while (and *running* (not (clatter.core.model:app-quit-requested app))) do
      ;; Process any pending events from other threads
      (process-events)
      
      ;; Check for input with timeout
      (let ((key (clatter.terminal:read-key-with-timeout 50)))
        (when key
          (handle-key app key)))
      
      ;; Render if dirty
      (when (clatter.core.model:dirty-p app)
        (render-frame app)))))

;; ui-submit macro is defined in submit.lisp
