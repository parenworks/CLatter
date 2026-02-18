(in-package #:clatter.core.dispatch)

(defgeneric apply-event (app event))
(defgeneric deliver-message (app buffer message &key highlightp))

(defun buffer-visible-p (app buf)
  "Check if buffer is currently visible (in left or right pane)."
  (let* ((buf-id (buffer-id buf))
         (ui (app-ui app))
         (left-id (app-current-buffer-id app))
         (split-mode (when ui (ui-split-mode ui)))
         (right-id (when split-mode (ui-split-buffer-id ui))))
    (or (eql buf-id left-id)
        (and right-id (eql buf-id right-id)))))

(defmethod deliver-message ((app app) (buf buffer) (msg message) &key (highlightp nil))
  (when highlightp
    (setf (clatter.core.model:message-highlight msg) t))
  (ring-push (buffer-scrollback buf) msg)
  ;; Log the message to disk
  (clatter.core.logging:log-message buf msg)
  ;; Extract and track URLs from message text
  (buffer-add-urls buf (clatter.core.model:message-text msg))
  ;; Only increment unread if buffer is not visible in either pane
  (if (buffer-visible-p app buf)
      ;; Visible buffer - update chat panel
      (mark-dirty app :chat :status)
      ;; Hidden buffer - only update buflist for unread count
      (progn
        (incf (buffer-unread-count buf))
        (when highlightp
          (incf (buffer-highlight-count buf)))
        (mark-dirty app :buflist))))

(defmethod apply-event ((app app) event)
  ;; default: ignore unknown events
  (declare (ignore event))
  app)
