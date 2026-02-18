(in-package #:clatter.ui.theme)

;;; Theme System for CLatter UI - CLOS-based
;;; Tokyo Night inspired color scheme

;;; ============================================================
;;; Base Theme Class
;;; ============================================================

(defclass base-theme ()
  (;; Nick colors for chat messages
   (nick-colors :initarg :nick-colors
                :accessor theme-nick-colors
                :initform nil)
   ;; Background and foreground
   (bg :initarg :bg :accessor theme-bg :initform nil)
   (fg :initarg :fg :accessor theme-fg :initform nil)
   ;; Border colors
   (border-active :initarg :border-active :accessor theme-border-active :initform nil)
   (border-inactive :initarg :border-inactive :accessor theme-border-inactive :initform nil)
   ;; Indicator colors
   (unread-indicator :initarg :unread-indicator :accessor theme-unread-indicator :initform nil)
   (mention-indicator :initarg :mention-indicator :accessor theme-mention-indicator :initform nil)
   (timestamp-color :initarg :timestamp :accessor theme-timestamp :initform nil)
   ;; Message level colors
   (join-color :initarg :join-color :accessor theme-join-color :initform nil)
   (part-color :initarg :part-color :accessor theme-part-color :initform nil)
   (error-color :initarg :error-color :accessor theme-error-color :initform nil)
   (system-color :initarg :system-color :accessor theme-system-color :initform nil)
   (presence-color :initarg :presence-color :accessor theme-presence-color :initform nil)
   (action-color :initarg :action-color :accessor theme-action-color :initform nil)
   ;; Input prompt
   (input-prompt :initarg :input-prompt :accessor theme-input-prompt :initform "> ")
   ;; Box drawing characters
   (box-h :initarg :box-h :accessor theme-box-h :initform #\─)
   (box-v :initarg :box-v :accessor theme-box-v :initform #\│)
   (box-tl :initarg :box-tl :accessor theme-box-tl :initform #\┌)
   (box-tr :initarg :box-tr :accessor theme-box-tr :initform #\┐)
   (box-bl :initarg :box-bl :accessor theme-box-bl :initform #\└)
   (box-br :initarg :box-br :accessor theme-box-br :initform #\┘)
   (box-t-down :initarg :box-t-down :accessor theme-box-t-down :initform #\┬)
   (box-t-up :initarg :box-t-up :accessor theme-box-t-up :initform #\┴)
   (box-t-right :initarg :box-t-right :accessor theme-box-t-right :initform #\├)
   (box-t-left :initarg :box-t-left :accessor theme-box-t-left :initform #\┤)
   (box-cross :initarg :box-cross :accessor theme-box-cross :initform #\┼)
   ;; Status indicators
   (connected-indicator :initarg :connected-indicator :accessor theme-connected-indicator :initform "●")
   (connecting-indicator :initarg :connecting-indicator :accessor theme-connecting-indicator :initform "⟳")
   (disconnected-indicator :initarg :disconnected-indicator :accessor theme-disconnected-indicator :initform "○")
   (tls-indicator :initarg :tls-indicator :accessor theme-tls-indicator :initform "🔒")
   (scroll-indicator :initarg :scroll-indicator :accessor theme-scroll-indicator :initform "↑")
   (filter-indicator :initarg :filter-indicator :accessor theme-filter-indicator :initform "🔍"))
  (:documentation "Base theme class defining all visual elements."))

;;; ============================================================
;;; Generic Functions for Theme Operations
;;; ============================================================

(defgeneric theme-level-color (theme level)
  (:documentation "Get the color for a message level from the theme."))

(defgeneric theme-nick-color (theme nick)
  (:documentation "Get a consistent color for a nick from the theme."))

(defmethod theme-level-color ((theme base-theme) level)
  (case level
    (:join (theme-join-color theme))
    (:part (theme-part-color theme))
    (:error (theme-error-color theme))
    (:system (theme-system-color theme))
    (:presence (theme-presence-color theme))
    (:action (theme-action-color theme))
    (otherwise nil)))

(defmethod theme-nick-color ((theme base-theme) nick)
  "Hash a nick to a consistent color from the theme's palette."
  (let* ((colors (theme-nick-colors theme))
         (hash (reduce #'+ (map 'list #'char-code (or nick "")))))
    (when (and colors (> (length colors) 0))
      (nth (mod hash (length colors)) colors))))

;;; ============================================================
;;; Tokyo Night Theme - Default
;;; ============================================================

(defclass tokyo-night-theme (base-theme)
  ()
  (:default-initargs
   ;; Tokyo Night color palette using RGB
   :bg (clatter.ansi:make-rgb-color #x1a #x1b #x26)
   :fg (clatter.ansi:make-rgb-color #xc0 #xca #xf5)
   :border-active (clatter.ansi:make-rgb-color #x7a #xa2 #xf7)
   :border-inactive (clatter.ansi:make-rgb-color #x3b #x42 #x61)
   :unread-indicator (clatter.ansi:make-rgb-color #xff #x9e #x64)
   :mention-indicator (clatter.ansi:make-rgb-color #xf7 #x76 #x8e)
   :timestamp (clatter.ansi:make-rgb-color #x56 #x5f #x89)
   :join-color (clatter.ansi:make-rgb-color #x9e #xce #x6a)
   :part-color (clatter.ansi:make-rgb-color #xe0 #xaf #x68)
   :error-color (clatter.ansi:make-rgb-color #xf7 #x76 #x8e)
   :system-color (clatter.ansi:make-rgb-color #x7a #xa2 #xf7)
   :presence-color (clatter.ansi:make-rgb-color #xbb #x9a #xf7)
   :action-color (clatter.ansi:make-rgb-color #xbb #x9a #xf7)
   :nick-colors (list
                 (clatter.ansi:make-rgb-color #x7a #xa2 #xf7)  ; blue
                 (clatter.ansi:make-rgb-color #x9e #xce #x6a)  ; green
                 (clatter.ansi:make-rgb-color #xe0 #xaf #x68)  ; amber
                 (clatter.ansi:make-rgb-color #xf7 #x76 #x8e)  ; pink
                 (clatter.ansi:make-rgb-color #xbb #x9a #xf7)  ; purple
                 (clatter.ansi:make-rgb-color #x7d #xcf #xff)  ; light blue
                 (clatter.ansi:make-rgb-color #xff #x9e #x64)  ; orange
                 (clatter.ansi:make-rgb-color #x2a #xc3 #xde)  ; teal
                 (clatter.ansi:make-rgb-color #xc0 #xca #xf5)  ; white
                 (clatter.ansi:make-rgb-color #x73 #xda #xca)  ; mint
                 (clatter.ansi:make-rgb-color #xb4 #xf9 #xf8)  ; cyan
                 (clatter.ansi:make-rgb-color #xff #x75 #x7f)) ; coral
   :input-prompt "❯ "))

;;; ============================================================
;;; Dark Theme - Simpler 256-color version
;;; ============================================================

(defclass dark-theme (base-theme)
  ()
  (:default-initargs
   :bg (clatter.ansi:make-indexed-color 234)
   :fg (clatter.ansi:make-indexed-color 252)
   :border-active (clatter.ansi:make-indexed-color 75)
   :border-inactive (clatter.ansi:make-indexed-color 240)
   :unread-indicator (clatter.ansi:make-indexed-color 208)
   :mention-indicator (clatter.ansi:make-indexed-color 197)
   :timestamp (clatter.ansi:make-indexed-color 245)
   :join-color (clatter.ansi:make-indexed-color 114)
   :part-color (clatter.ansi:make-indexed-color 179)
   :error-color (clatter.ansi:make-indexed-color 197)
   :system-color (clatter.ansi:make-indexed-color 75)
   :presence-color (clatter.ansi:make-indexed-color 141)
   :action-color (clatter.ansi:make-indexed-color 141)
   :nick-colors (list
                 (clatter.ansi:make-indexed-color 75)
                 (clatter.ansi:make-indexed-color 114)
                 (clatter.ansi:make-indexed-color 179)
                 (clatter.ansi:make-indexed-color 197)
                 (clatter.ansi:make-indexed-color 141)
                 (clatter.ansi:make-indexed-color 117)
                 (clatter.ansi:make-indexed-color 208)
                 (clatter.ansi:make-indexed-color 80)
                 (clatter.ansi:make-indexed-color 252)
                 (clatter.ansi:make-indexed-color 121)
                 (clatter.ansi:make-indexed-color 159)
                 (clatter.ansi:make-indexed-color 210))
   :input-prompt "> "))

;;; ============================================================
;;; Light Theme
;;; ============================================================

(defclass light-theme (base-theme)
  ()
  (:default-initargs
   :bg (clatter.ansi:make-indexed-color 231)
   :fg (clatter.ansi:make-indexed-color 235)
   :border-active (clatter.ansi:make-indexed-color 33)
   :border-inactive (clatter.ansi:make-indexed-color 250)
   :unread-indicator (clatter.ansi:make-indexed-color 166)
   :mention-indicator (clatter.ansi:make-indexed-color 161)
   :timestamp (clatter.ansi:make-indexed-color 245)
   :join-color (clatter.ansi:make-indexed-color 28)
   :part-color (clatter.ansi:make-indexed-color 130)
   :error-color (clatter.ansi:make-indexed-color 160)
   :system-color (clatter.ansi:make-indexed-color 33)
   :presence-color (clatter.ansi:make-indexed-color 91)
   :action-color (clatter.ansi:make-indexed-color 91)
   :nick-colors (list
                 (clatter.ansi:make-indexed-color 33)
                 (clatter.ansi:make-indexed-color 28)
                 (clatter.ansi:make-indexed-color 130)
                 (clatter.ansi:make-indexed-color 161)
                 (clatter.ansi:make-indexed-color 91)
                 (clatter.ansi:make-indexed-color 31)
                 (clatter.ansi:make-indexed-color 166)
                 (clatter.ansi:make-indexed-color 30)
                 (clatter.ansi:make-indexed-color 238)
                 (clatter.ansi:make-indexed-color 29)
                 (clatter.ansi:make-indexed-color 37)
                 (clatter.ansi:make-indexed-color 167))
   :input-prompt "> "))

;;; ============================================================
;;; ASCII Theme - No Unicode box drawing
;;; ============================================================

(defclass ascii-theme (dark-theme)
  ()
  (:default-initargs
   :box-h #\-
   :box-v #\|
   :box-tl #\+
   :box-tr #\+
   :box-bl #\+
   :box-br #\+
   :box-t-down #\+
   :box-t-up #\+
   :box-t-right #\+
   :box-t-left #\+
   :box-cross #\+
   :connected-indicator "*"
   :connecting-indicator "~"
   :disconnected-indicator "o"
   :tls-indicator "[TLS]"
   :scroll-indicator "^"
   :filter-indicator "[F]"
   :input-prompt "> "))

;;; ============================================================
;;; Rounded Theme - Rounded box corners
;;; ============================================================

(defclass rounded-theme (tokyo-night-theme)
  ()
  (:default-initargs
   :box-tl #\╭
   :box-tr #\╮
   :box-bl #\╰
   :box-br #\╯))

;;; ============================================================
;;; Theme Registry
;;; ============================================================

(defvar *theme-registry* (make-hash-table :test 'eq))
(defvar *current-theme* nil)

(defun register-theme (name theme-class)
  "Register a theme class under a name."
  (setf (gethash name *theme-registry*) theme-class))

(defun find-theme (name)
  "Find a theme class by name."
  (gethash name *theme-registry*))

(defun list-themes ()
  "List all registered theme names."
  (let ((names nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *theme-registry*)
    (sort names #'string< :key #'symbol-name)))

(defun current-theme ()
  "Get the current theme instance."
  (or *current-theme*
      (setf *current-theme* (make-instance 'tokyo-night-theme))))

(defun set-theme (name)
  "Set the current theme by name."
  (let ((theme-class (find-theme name)))
    (if theme-class
        (setf *current-theme* (make-instance theme-class))
        (warn "Unknown theme: ~A" name))))

;; Register default themes
(register-theme :tokyo-night 'tokyo-night-theme)
(register-theme :dark 'dark-theme)
(register-theme :light 'light-theme)
(register-theme :ascii 'ascii-theme)
(register-theme :rounded 'rounded-theme)
