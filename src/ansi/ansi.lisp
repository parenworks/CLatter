(in-package #:clatter.ansi)

;;; ANSI Escape Sequence Library - CLOS-based
;;; Direct terminal control without ncurses dependency

(defparameter *escape* (code-char 27))

;;; ============================================================
;;; Color Class Hierarchy
;;; ============================================================

(defclass color ()
  ()
  (:documentation "Base class for terminal colors"))

(defclass indexed-color (color)
  ((index :initarg :index :accessor color-index :type (integer 0 255)))
  (:documentation "256-color palette color"))

(defclass rgb-color (color)
  ((red :initarg :red :accessor color-red :type (integer 0 255))
   (green :initarg :green :accessor color-green :type (integer 0 255))
   (blue :initarg :blue :accessor color-blue :type (integer 0 255)))
  (:documentation "24-bit true color"))

(defclass named-color (indexed-color)
  ((name :initarg :name :accessor color-name :type keyword))
  (:documentation "Named color with semantic meaning"))

;;; Color constructors

(defun make-indexed-color (index)
  (make-instance 'indexed-color :index index))

(defun make-rgb-color (r g b)
  (make-instance 'rgb-color :red r :green g :blue b))

(defun make-named-color (name index)
  (make-instance 'named-color :name name :index index))

;;; Standard color palette

(defparameter *color-palette* (make-hash-table :test 'eq))

(defun register-color (name index)
  (setf (gethash name *color-palette*) (make-named-color name index)))

(defun lookup-color (name-or-index)
  "Get a color object from name or index"
  (etypecase name-or-index
    (color name-or-index)
    (keyword (or (gethash name-or-index *color-palette*)
                 (make-indexed-color 7)))
    (integer (make-indexed-color name-or-index))
    (null nil)))

;; Register standard colors
(register-color :black 0)
(register-color :red 1)
(register-color :green 2)
(register-color :yellow 3)
(register-color :blue 4)
(register-color :magenta 5)
(register-color :cyan 6)
(register-color :white 7)
(register-color :bright-black 8)
(register-color :bright-red 9)
(register-color :bright-green 10)
(register-color :bright-yellow 11)
(register-color :bright-blue 12)
(register-color :bright-magenta 13)
(register-color :bright-cyan 14)
(register-color :bright-white 15)

;;; ============================================================
;;; Generic Functions for Color Output
;;; ============================================================

(defgeneric emit-fg (color stream)
  (:documentation "Emit ANSI sequence for foreground color"))

(defgeneric emit-bg (color stream)
  (:documentation "Emit ANSI sequence for background color"))

(defmethod emit-fg ((color indexed-color) stream)
  (format stream "~C[38;5;~Dm" *escape* (color-index color)))

(defmethod emit-fg ((color rgb-color) stream)
  (format stream "~C[38;2;~D;~D;~Dm" *escape* 
          (color-red color) (color-green color) (color-blue color)))

(defmethod emit-fg ((color null) stream)
  (declare (ignore stream))
  nil)

(defmethod emit-bg ((color indexed-color) stream)
  (format stream "~C[48;5;~Dm" *escape* (color-index color)))

(defmethod emit-bg ((color rgb-color) stream)
  (format stream "~C[48;2;~D;~D;~Dm" *escape*
          (color-red color) (color-green color) (color-blue color)))

(defmethod emit-bg ((color null) stream)
  (declare (ignore stream))
  nil)

;;; ============================================================
;;; Text Style Class
;;; ============================================================

(defclass text-style ()
  ((foreground :initarg :fg :accessor style-fg :initform nil)
   (background :initarg :bg :accessor style-bg :initform nil)
   (bold-p :initarg :bold :accessor style-bold-p :initform nil)
   (dim-p :initarg :dim :accessor style-dim-p :initform nil)
   (italic-p :initarg :italic :accessor style-italic-p :initform nil)
   (underline-p :initarg :underline :accessor style-underline-p :initform nil)
   (inverse-p :initarg :inverse :accessor style-inverse-p :initform nil))
  (:documentation "Text styling attributes"))

(defun make-style (&key fg bg bold dim italic underline inverse)
  (make-instance 'text-style
                 :fg (when fg (lookup-color fg))
                 :bg (when bg (lookup-color bg))
                 :bold bold :dim dim :italic italic
                 :underline underline :inverse inverse))

(defgeneric emit-style (style stream)
  (:documentation "Emit ANSI sequences for a style"))

(defmethod emit-style ((style text-style) stream)
  (when (style-bold-p style) (format stream "~C[1m" *escape*))
  (when (style-dim-p style) (format stream "~C[2m" *escape*))
  (when (style-italic-p style) (format stream "~C[3m" *escape*))
  (when (style-underline-p style) (format stream "~C[4m" *escape*))
  (when (style-inverse-p style) (format stream "~C[7m" *escape*))
  (when (style-fg style) (emit-fg (style-fg style) stream))
  (when (style-bg style) (emit-bg (style-bg style) stream)))

(defmethod emit-style ((style null) stream)
  (declare (ignore stream))
  nil)

;;; ============================================================
;;; Cursor and Screen Control Functions
;;; ============================================================

(defun cursor-to (row col)
  "Move cursor to 1-indexed position"
  (format *terminal-io* "~C[~D;~DH" *escape* (max 1 row) (max 1 col)))

(defun cursor-home ()
  (format *terminal-io* "~C[H" *escape*))

(defun cursor-hide ()
  (format *terminal-io* "~C[?25l" *escape*))

(defun cursor-show ()
  (format *terminal-io* "~C[?25h" *escape*))

(defun cursor-up (&optional (n 1))
  (format *terminal-io* "~C[~DA" *escape* n))

(defun cursor-down (&optional (n 1))
  (format *terminal-io* "~C[~DB" *escape* n))

(defun cursor-forward (&optional (n 1))
  (format *terminal-io* "~C[~DC" *escape* n))

(defun cursor-back (&optional (n 1))
  (format *terminal-io* "~C[~DD" *escape* n))

(defun clear-screen ()
  (format *terminal-io* "~C[2J" *escape*))

(defun clear-line ()
  (format *terminal-io* "~C[2K" *escape*))

(defun clear-to-eol ()
  (format *terminal-io* "~C[K" *escape*))

(defun reset ()
  "Reset all text attributes"
  (format *terminal-io* "~C[0m" *escape*))

(defun bold ()
  "Enable bold text"
  (format *terminal-io* "~C[1m" *escape*))

(defun underline ()
  "Enable underlined text"
  (format *terminal-io* "~C[4m" *escape*))

(defun inverse ()
  "Enable inverse/reverse video"
  (format *terminal-io* "~C[7m" *escape*))

(defun begin-sync-update ()
  "Begin synchronized update mode - terminal buffers output until end-sync-update"
  (format *terminal-io* "~C[?2026h" *escape*))

(defun end-sync-update ()
  "End synchronized update mode - terminal displays buffered content"
  (format *terminal-io* "~C[?2026l" *escape*))

(defun begin-hyperlink (url)
  "Begin an OSC 8 hyperlink. Text after this will be clickable.
   Format: ESC ] 8 ; ; URL ST where ST is ESC \\"
  (format *terminal-io* "~C]8;;~A~C\\" *escape* url *escape*))

(defun end-hyperlink ()
  "End an OSC 8 hyperlink."
  (format *terminal-io* "~C]8;;~C\\" *escape* *escape*))

(defun hyperlink (url text)
  "Output TEXT as a clickable hyperlink to URL."
  (begin-hyperlink url)
  (princ text *terminal-io*)
  (end-hyperlink))

(defun enter-alternate-screen ()
  "Switch to alternate screen buffer"
  (format *terminal-io* "~C[?1049h" *escape*)
  (force-output *terminal-io*))

(defun leave-alternate-screen ()
  "Switch back to main screen buffer"
  (format *terminal-io* "~C[?1049l" *escape*)
  (force-output *terminal-io*))

;;; ============================================================
;;; Color Convenience Functions
;;; ============================================================

(defun fg (color)
  "Set foreground color"
  (let ((c (lookup-color color)))
    (when c (emit-fg c *terminal-io*))))

(defun bg (color)
  "Set background color"
  (let ((c (lookup-color color)))
    (when c (emit-bg c *terminal-io*))))

(defun fg-rgb (r g b)
  (emit-fg (make-rgb-color r g b) *terminal-io*))

(defun bg-rgb (r g b)
  (emit-bg (make-rgb-color r g b) *terminal-io*))

(defun bold () (format *terminal-io* "~C[1m" *escape*))
(defun dim () (format *terminal-io* "~C[2m" *escape*))
(defun italic () (format *terminal-io* "~C[3m" *escape*))
(defun underline () (format *terminal-io* "~C[4m" *escape*))
(defun inverse () (format *terminal-io* "~C[7m" *escape*))

;;; ============================================================
;;; Styled Output
;;; ============================================================

(defun write-at (row col text &key style)
  "Write text at position with optional style"
  (cursor-to row col)
  (when style (emit-style style *terminal-io*))
  (princ text *terminal-io*)
  (when style (reset)))

(defun write-styled (text style)
  "Write text with style at current position"
  (when style (emit-style style *terminal-io*))
  (princ text *terminal-io*)
  (when style (reset)))

(defmacro with-style ((&key fg bg bold dim italic underline inverse) &body body)
  "Execute body with specified text styling, then reset"
  `(progn
     ,@(when bold '((bold)))
     ,@(when dim '((dim)))
     ,@(when italic '((italic)))
     ,@(when underline '((underline)))
     ,@(when inverse '((inverse)))
     ,@(when fg `((fg ,fg)))
     ,@(when bg `((bg ,bg)))
     (unwind-protect
          (progn ,@body)
       (reset))))

;;; ============================================================
;;; Box Drawing
;;; ============================================================

(defun draw-box (x y width height &key (style nil) 
                                       (tl #\┌) (tr #\┐) (bl #\└) (br #\┘)
                                       (h #\─) (v #\│))
  "Draw a box at position with given dimensions"
  (when style (emit-style style *terminal-io*))
  ;; Top border
  (cursor-to y x)
  (princ tl *terminal-io*)
  (loop repeat (- width 2) do (princ h *terminal-io*))
  (princ tr *terminal-io*)
  ;; Side borders
  (loop for row from (1+ y) below (+ y height -1) do
    (cursor-to row x)
    (princ v *terminal-io*)
    (cursor-to row (+ x width -1))
    (princ v *terminal-io*))
  ;; Bottom border
  (cursor-to (+ y height -1) x)
  (princ bl *terminal-io*)
  (loop repeat (- width 2) do (princ h *terminal-io*))
  (princ br *terminal-io*)
  (when style (reset)))

(defun fill-rect (x y width height &optional (char #\Space))
  "Fill a rectangle with a character"
  (let ((line (make-string width :initial-element char)))
    (loop for row from y below (+ y height) do
      (cursor-to row x)
      (princ line *terminal-io*))))
