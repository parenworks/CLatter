(in-package #:clatter.ui.tui)

;;; Thread-safe UI submission mechanism
;;; This file is loaded early so the ui-submit macro is available
;;; to other modules that need to update the UI from background threads.

(defvar *event-queue* nil "Thread-safe event queue")
(defvar *event-lock* (bordeaux-threads:make-lock "event-queue"))
(defvar *event-cv* (bordeaux-threads:make-condition-variable :name "event-cv"))

(defun submit-event (thunk)
  "Submit a function to be executed on the main UI thread.
   This replaces croatoan:submit for thread-safe UI updates."
  (bordeaux-threads:with-lock-held (*event-lock*)
    (push thunk *event-queue*)
    (bordeaux-threads:condition-notify *event-cv*)))

(defmacro ui-submit (&body body)
  "Submit code to run on the UI thread.
   Use this from IRC handler threads to update the UI safely.
   This is a macro to match croatoan:submit's interface."
  `(submit-event (lambda () ,@body)))
