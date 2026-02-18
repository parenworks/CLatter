(in-package #:clatter.ui.layout)

;;; Layout Manager for CLatter UI - CLOS-based
;;; Computes panel positions and dimensions based on terminal size

;;; ============================================================
;;; Layout Class
;;; ============================================================

(defclass layout ()
  ((buflist-width :initarg :buflist-width :accessor layout-buflist-width 
                  :initform clatter.core.constants:+default-buflist-width+)
   (nicklist-width :initarg :nicklist-width :accessor layout-nicklist-width
                   :initform clatter.core.constants:+default-nicklist-width+)
   (nicklist-visible :initarg :nicklist-visible :accessor layout-nicklist-visible :initform nil)
   (split-mode :initarg :split-mode :accessor layout-split-mode :initform nil
               :documentation "Split mode: nil (single), :horizontal, :vertical")
   (term-width :accessor layout-term-width :initform 80)
   (term-height :accessor layout-term-height :initform 24)
   ;; Panel instances
   (buflist :accessor layout-buflist :initform nil)
   (chat-a :accessor layout-chat-a :initform nil)
   (chat-b :accessor layout-chat-b :initform nil)
   (nicklist :accessor layout-nicklist :initform nil)
   (status :accessor layout-status :initform nil)
   (input :accessor layout-input :initform nil))
  (:documentation "Screen layout manager"))

(defun make-layout (&key (buflist-width clatter.core.constants:+default-buflist-width+)
                         (nicklist-width clatter.core.constants:+default-nicklist-width+)
                         nicklist-visible split-mode)
  (make-instance 'layout
                 :buflist-width buflist-width
                 :nicklist-width nicklist-width
                 :nicklist-visible nicklist-visible
                 :split-mode split-mode))

;;; ============================================================
;;; Layout Computation
;;; ============================================================

(defgeneric layout-compute (layout width height app)
  (:documentation "Compute panel positions and dimensions"))

(defmethod layout-compute ((layout layout) width height app)
  "Compute panel positions for the given terminal size.
   Layout: input at row 1, status at row 2, chat starts at row 3,
   buflist on left full height, nicklist on right if visible."
  (setf (layout-term-width layout) width
        (layout-term-height layout) height)
  (let* ((buflist-w (min (layout-buflist-width layout) (floor width 3)))
         (nicklist-visible (layout-nicklist-visible layout))
         (nicklist-w (if nicklist-visible (layout-nicklist-width layout) 0))
         (chat-x (1+ buflist-w))
         (chat-w (- width buflist-w nicklist-w (if nicklist-visible 1 0)))
         (input-y 1)
         (status-y 2)
         (chat-y 3)
         (chat-h (- height 2))
         (split (layout-split-mode layout)))
    ;; Input bar at top (row 1)
    (setf (layout-input layout)
          (make-instance 'clatter.ui.widgets:input-panel
                         :x chat-x :y input-y
                         :width (- width buflist-w) :height 1
                         :input-state (clatter.core.model:ui-input 
                                       (clatter.core.model:app-ui app))))
    ;; Status bar (row 2)
    (setf (layout-status layout)
          (make-instance 'clatter.ui.widgets:status-panel
                         :x chat-x :y status-y
                         :width (- width buflist-w) :height 1
                         :app app
                         :buffer (clatter.core.model:current-buffer app)))
    ;; Buffer list panel (full height on left)
    (setf (layout-buflist layout)
          (make-instance 'clatter.ui.widgets:buflist-panel
                         :x 1 :y 1
                         :width buflist-w :height height
                         :app app))
    ;; Nick list panel (right side if visible)
    (if nicklist-visible
        (setf (layout-nicklist layout)
              (make-instance 'clatter.ui.widgets:nicklist-panel
                             :x (- width nicklist-w) :y chat-y
                             :width nicklist-w :height chat-h
                             :buffer (clatter.core.model:active-buffer app)))
        (setf (layout-nicklist layout) nil))
    ;; Chat panel(s) based on split mode
    (let ((ui (clatter.core.model:app-ui app))
          (active-pane (clatter.core.model:ui-active-pane (clatter.core.model:app-ui app))))
      (cond
        ;; Horizontal split: two panes side by side
        ((eq split :horizontal)
         (let* ((pane-w (floor (- chat-w 1) 2))
                (pane2-x (+ chat-x pane-w 1)))
           (setf (layout-chat-a layout)
                 (make-instance 'clatter.ui.widgets:chat-panel
                                :x chat-x :y chat-y
                                :width pane-w :height chat-h
                                :buffer (clatter.core.model:current-buffer app)
                                :active (member active-pane '(:left :top))
                                :title (format nil " ~a " 
                                               (clatter.core.model:buffer-title 
                                                (clatter.core.model:current-buffer app)))))
           (let ((split-buf-id (clatter.core.model:ui-split-buffer-id ui)))
             (when (and split-buf-id 
                        (< split-buf-id (length (clatter.core.model:app-buffers app))))
               (let ((split-buf (aref (clatter.core.model:app-buffers app) split-buf-id)))
                 (setf (layout-chat-b layout)
                       (make-instance 'clatter.ui.widgets:chat-panel
                                      :x pane2-x :y chat-y
                                      :width pane-w :height chat-h
                                      :buffer split-buf
                                      :active (member active-pane '(:right :bottom))
                                      :title (format nil " ~a " 
                                                     (clatter.core.model:buffer-title split-buf)))))))))
        ;; Vertical split: two panes stacked
        ((eq split :vertical)
         (let* ((pane-h (floor (- chat-h 1) 2))
                (pane2-y (+ chat-y pane-h 1)))
           (setf (layout-chat-a layout)
                 (make-instance 'clatter.ui.widgets:chat-panel
                                :x chat-x :y chat-y
                                :width chat-w :height pane-h
                                :buffer (clatter.core.model:current-buffer app)
                                :active (member active-pane '(:left :top))
                                :title (format nil " ~a "
                                               (clatter.core.model:buffer-title
                                                (clatter.core.model:current-buffer app)))))
           (let ((split-buf-id (clatter.core.model:ui-split-buffer-id ui)))
             (when (and split-buf-id
                        (< split-buf-id (length (clatter.core.model:app-buffers app))))
               (let ((split-buf (aref (clatter.core.model:app-buffers app) split-buf-id)))
                 (setf (layout-chat-b layout)
                       (make-instance 'clatter.ui.widgets:chat-panel
                                      :x chat-x :y pane2-y
                                      :width chat-w :height pane-h
                                      :buffer split-buf
                                      :active (member active-pane '(:right :bottom))
                                      :title (format nil " ~a "
                                                     (clatter.core.model:buffer-title split-buf)))))))))
        ;; Single pane
        (t
         (setf (layout-chat-a layout)
               (make-instance 'clatter.ui.widgets:chat-panel
                              :x chat-x :y chat-y
                              :width chat-w :height chat-h
                              :buffer (clatter.core.model:current-buffer app)))
         (setf (layout-chat-b layout) nil))))
    layout))

;;; ============================================================
;;; Layout Rendering
;;; ============================================================

(defgeneric layout-render (layout)
  (:documentation "Render all panels in the layout"))

(defmethod layout-render ((layout layout))
  "Render all panels with synchronized update."
  (clatter.ansi:begin-sync-update)
  (when (layout-buflist layout) 
    (clatter.ui.widgets:panel-render (layout-buflist layout)))
  (when (layout-chat-a layout) 
    (clatter.ui.widgets:panel-render (layout-chat-a layout)))
  (when (layout-chat-b layout) 
    (clatter.ui.widgets:panel-render (layout-chat-b layout)))
  (when (layout-nicklist layout) 
    (clatter.ui.widgets:panel-render (layout-nicklist layout)))
  (when (layout-status layout) 
    (clatter.ui.widgets:panel-render (layout-status layout)))
  (when (layout-input layout) 
    (clatter.ui.widgets:panel-render (layout-input layout)))
  (clatter.ansi:end-sync-update)
  (force-output *terminal-io*))

;;; ============================================================
;;; Layout Update Helpers
;;; ============================================================

(defun layout-update-buffers (layout app)
  "Update buffer references in panels after buffer changes."
  (when (layout-status layout)
    (setf (clatter.ui.widgets:status-buffer (layout-status layout))
          (clatter.core.model:current-buffer app)))
  (when (layout-chat-a layout)
    (setf (clatter.ui.widgets:chat-buffer (layout-chat-a layout))
          (clatter.core.model:current-buffer app)))
  (when (layout-nicklist layout)
    (setf (clatter.ui.widgets:nicklist-buffer (layout-nicklist layout))
          (clatter.core.model:active-buffer app)))
  (let ((ui (clatter.core.model:app-ui app)))
    (when (and (layout-chat-b layout) (clatter.core.model:ui-split-buffer-id ui))
      (let ((split-buf-id (clatter.core.model:ui-split-buffer-id ui)))
        (when (< split-buf-id (length (clatter.core.model:app-buffers app)))
          (setf (clatter.ui.widgets:chat-buffer (layout-chat-b layout))
                (aref (clatter.core.model:app-buffers app) split-buf-id)))))))
