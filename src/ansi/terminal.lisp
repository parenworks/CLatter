(in-package #:clatter.terminal)

;;; Raw Terminal Input/Output - CLOS-based
;;; Handles putting terminal in raw mode and reading key events

(require :sb-posix)

;;; ============================================================
;;; Key Event Class
;;; ============================================================

(defclass key-event ()
  ((char :initarg :char :accessor key-event-char :initform nil
         :documentation "Character if printable key")
   (code :initarg :code :accessor key-event-code :initform nil
         :documentation "Keyword for special keys")
   (ctrl-p :initarg :ctrl-p :accessor key-event-ctrl-p :initform nil
           :documentation "Control modifier pressed")
   (alt-p :initarg :alt-p :accessor key-event-alt-p :initform nil
          :documentation "Alt modifier pressed")
   (mouse-x :initarg :mouse-x :accessor key-event-mouse-x :initform nil)
   (mouse-y :initarg :mouse-y :accessor key-event-mouse-y :initform nil))
  (:documentation "Represents a keyboard input event"))

(defmethod print-object ((key key-event) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~@[char=~S~]~@[ code=~S~]~@[ ctrl~]~@[ alt~]"
            (key-event-char key) (key-event-code key)
            (key-event-ctrl-p key) (key-event-alt-p key))))

(defun make-key-event (&key char code ctrl-p alt-p mouse-x mouse-y)
  (make-instance 'key-event :char char :code code :ctrl-p ctrl-p :alt-p alt-p
                            :mouse-x mouse-x :mouse-y mouse-y))

;;; Special key codes
(defconstant +key-up+ :up)
(defconstant +key-down+ :down)
(defconstant +key-left+ :left)
(defconstant +key-right+ :right)
(defconstant +key-enter+ :enter)
(defconstant +key-escape+ :escape)
(defconstant +key-tab+ :tab)
(defconstant +key-backspace+ :backspace)
(defconstant +key-delete+ :delete)
(defconstant +key-home+ :home)
(defconstant +key-end+ :end)
(defconstant +key-page-up+ :page-up)
(defconstant +key-page-down+ :page-down)
(defconstant +key-mouse+ :mouse)
(defconstant +key-resize+ :resize)

;;; ============================================================
;;; Environment Configuration
;;; ============================================================

(defparameter *tty-path*
  (or (sb-ext:posix-getenv "CLATTER_TTY_PATH")
      (let ((candidates '("/dev/tty" "/dev/pts/0" "/dev/console")))
        (loop for path in candidates
              when (ignore-errors (probe-file path))
              return path
              finally (return "/dev/tty")))))

(defparameter *escape-timeout*
  (or (ignore-errors 
        (let ((timeout-str (sb-ext:posix-getenv "CLATTER_ESCAPE_TIMEOUT")))
          (when timeout-str 
            (let ((parsed (read-from-string timeout-str)))
              (if (numberp parsed) parsed nil)))))
      (let ((term (sb-ext:posix-getenv "TERM"))
            (alacritty-socket (sb-ext:posix-getenv "ALACRITTY_SOCKET")))
        (cond
          (alacritty-socket 0.01)
          ((and term (search "alacritty" term)) 0.01)
          (t 0.02)))))

;;; ============================================================
;;; Terminal Mode Controller Class
;;; ============================================================

(defclass terminal-mode ()
  ((raw-p :initarg :raw-p :accessor terminal-raw-p :initform nil)
   (original-settings :accessor terminal-original-settings :initform nil)
   (width :accessor terminal-width :initform 80)
   (height :accessor terminal-height :initform 24))
  (:documentation "Manages terminal mode state"))

(defgeneric enable-raw-mode (mode)
  (:documentation "Put terminal in raw mode"))

(defgeneric disable-raw-mode (mode)
  (:documentation "Restore terminal to normal mode"))

(defgeneric query-size (mode)
  (:documentation "Query terminal dimensions"))

(defmethod enable-raw-mode ((mode terminal-mode))
  (unless (terminal-raw-p mode)
    (handler-case
        (let* ((fd (sb-sys:fd-stream-fd sb-sys:*stdin*))
               (orig (sb-posix:tcgetattr fd))
               (raw (sb-posix:tcgetattr fd)))
          ;; Save original for restore
          (setf (terminal-original-settings mode) orig)
          ;; Modify flags for raw mode
          (setf (sb-posix:termios-iflag raw)
                (logand (sb-posix:termios-iflag raw)
                        (lognot (logior sb-posix:brkint sb-posix:icrnl
                                       sb-posix:inpck sb-posix:istrip sb-posix:ixon))))
          (setf (sb-posix:termios-oflag raw)
                (logand (sb-posix:termios-oflag raw)
                        (lognot sb-posix:opost)))
          (setf (sb-posix:termios-cflag raw)
                (logior (sb-posix:termios-cflag raw) sb-posix:cs8))
          (setf (sb-posix:termios-lflag raw)
                (logand (sb-posix:termios-lflag raw)
                        (lognot (logior sb-posix:echo sb-posix:icanon
                                       sb-posix:iexten sb-posix:isig))))
          ;; VMIN=1 VTIME=0
          (let ((cc (sb-posix:termios-cc raw)))
            (setf (aref cc sb-posix:vmin) 1)
            (setf (aref cc sb-posix:vtime) 0))
          (sb-posix:tcsetattr fd sb-posix:tcsaflush raw))
      (error (e)
        (warn "Failed to enable raw mode: ~A" e)))
    (setf (terminal-raw-p mode) t)))

(defmethod disable-raw-mode ((mode terminal-mode))
  (when (terminal-raw-p mode)
    (handler-case
        (let ((saved (terminal-original-settings mode)))
          (when saved
            (sb-posix:tcsetattr (sb-sys:fd-stream-fd sb-sys:*stdin*)
                                sb-posix:tcsaflush saved)))
      (error (e)
        (warn "Failed to disable raw mode: ~A" e)))
    (setf (terminal-raw-p mode) nil)))

(defmethod query-size ((mode terminal-mode))
  (handler-case
      (sb-alien:with-alien ((buf (sb-alien:array (sb-alien:unsigned 8) 8)))
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "ioctl"
                                (function sb-alien:int sb-alien:int
                                          sb-alien:unsigned-long (* t)))
         (sb-sys:fd-stream-fd sb-sys:*stdin*)
         #x5413  ; TIOCGWINSZ
         (sb-alien:addr (sb-alien:deref buf 0)))
        (let ((rows (logior (sb-alien:deref buf 0) (ash (sb-alien:deref buf 1) 8)))
              (cols (logior (sb-alien:deref buf 2) (ash (sb-alien:deref buf 3) 8))))
          (when (and (> rows 0) (> cols 0))
            (setf (terminal-width mode) cols
                  (terminal-height mode) rows)
            (list cols rows))))
    (error (e)
      (declare (ignore e))
      (list 80 24))))

;;; Global terminal mode instance
(defparameter *terminal-mode* (make-instance 'terminal-mode))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(defun terminal-size ()
  "Return (width height) of terminal"
  (query-size *terminal-mode*))

(defun terminal-width ()
  (first (terminal-size)))

(defun terminal-height ()
  (second (terminal-size)))

(defun enable-mouse-tracking ()
  "Enable mouse tracking (SGR extended coordinates)"
  (format *terminal-io* "~C[?1000h~C[?1002h~C[?1006h" 
          clatter.ansi:*escape* clatter.ansi:*escape* clatter.ansi:*escape*)
  (force-output *terminal-io*))

(defun disable-mouse-tracking ()
  "Disable mouse tracking"
  (format *terminal-io* "~C[?1006l~C[?1002l~C[?1000l" 
          clatter.ansi:*escape* clatter.ansi:*escape* clatter.ansi:*escape*)
  (force-output *terminal-io*))

(defmacro with-raw-terminal (&body body)
  "Execute body with terminal in raw mode, ensuring cleanup"
  `(progn
     (clatter.ansi:enter-alternate-screen)
     (enable-raw-mode *terminal-mode*)
     (enable-mouse-tracking)
     (clatter.ansi:cursor-hide)
     (unwind-protect
          (progn ,@body)
       (close-tty-stream)
       (disable-mouse-tracking)
       (clatter.ansi:cursor-show)
       (disable-raw-mode *terminal-mode*)
       (clatter.ansi:leave-alternate-screen)
       (clatter.ansi:reset))))

;;; ============================================================
;;; Input Reader Class
;;; ============================================================

(defclass input-reader ()
  ((stream :initarg :stream :accessor reader-stream :initform nil)
   (tty-path :initarg :tty-path :accessor reader-tty-path :initform *tty-path*))
  (:documentation "Reads and parses keyboard input from TTY"))

(defmethod print-object ((reader input-reader) stream)
  (print-unreadable-object (reader stream :type t)
    (format stream "~A ~:[closed~;open~]"
            (reader-tty-path reader)
            (and (reader-stream reader) (open-stream-p (reader-stream reader))))))

(defgeneric reader-open (reader)
  (:documentation "Open the TTY stream for reading"))

(defgeneric reader-close (reader)
  (:documentation "Close the TTY stream"))

(defgeneric read-key-event (reader)
  (:documentation "Read a key event from the input stream"))

(defmethod reader-open ((reader input-reader))
  (unless (and (reader-stream reader) (open-stream-p (reader-stream reader)))
    (setf (reader-stream reader)
          (open (reader-tty-path reader)
                :direction :input
                :element-type '(unsigned-byte 8)
                :if-does-not-exist :error))
    ;; Set non-blocking mode
    (let ((fd (sb-sys:fd-stream-fd (reader-stream reader))))
      (sb-posix:fcntl fd sb-posix:f-setfl 
                      (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                              sb-posix:o-nonblock))))
  reader)

(defmethod reader-close ((reader input-reader))
  (when (and (reader-stream reader) (open-stream-p (reader-stream reader)))
    (close (reader-stream reader))
    (setf (reader-stream reader) nil))
  reader)

(defun read-byte-from-fd (fd)
  "Read a single byte directly from file descriptor.
   Returns the byte or nil if no data available."
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buf))
    (let ((n (sb-unix:unix-read fd (sb-sys:vector-sap buf) 1)))
      (cond
        ((and n (= n 1)) (aref buf 0))
        (t nil)))))

(defun wait-for-escape-sequence (stream timeout)
  "Wait for escape sequence bytes with timeout.
   Returns the next byte if available within timeout, or nil."
  (let ((fd (sb-sys:fd-stream-fd stream))
        (start (get-internal-real-time))
        (timeout-ticks (* timeout internal-time-units-per-second)))
    (loop
      (let ((byte (read-byte-from-fd fd)))
        (when byte (return byte)))
      (when (>= (- (get-internal-real-time) start) timeout-ticks)
        (return nil))
      (sleep 0.001))))

(defmethod read-key-event ((reader input-reader))
  "Read a key event from the TTY."
  (let* ((stream (reader-stream reader))
         (fd (sb-sys:fd-stream-fd stream))
         (byte (read-byte-from-fd fd)))
    (unless byte
      (return-from read-key-event nil))
    (cond
      ;; Escape sequence or bare escape
      ((= byte 27)
       (let ((next (wait-for-escape-sequence stream *escape-timeout*)))
         (cond
           ((null next)
            (make-key-event :code +key-escape+))
           ((= next 91)  ; CSI: ESC [
            (parse-csi-sequence fd))
           (t
            ;; Alt + key
            (make-key-event :char (code-char next) :alt-p t)))))
      ;; Control characters
      ((< byte 32)
       (cond
         ((= byte 13) (make-key-event :code +key-enter+))
         ((= byte 10) (make-key-event :char #\Newline))
         ((= byte 9) (make-key-event :code +key-tab+))
         ((= byte 8) (make-key-event :code +key-backspace+))
         (t (make-key-event :char (code-char (+ byte 96)) :ctrl-p t))))
      ;; DEL character (127) - backspace
      ((= byte 127)
       (make-key-event :code +key-backspace+))
      ;; Regular character (handle UTF-8)
      (t
       (make-key-event :char (decode-utf8-char byte fd))))))

(defun decode-utf8-char (first-byte fd)
  "Decode a UTF-8 character starting with FIRST-BYTE."
  (cond
    ;; ASCII
    ((< first-byte 128)
     (code-char first-byte))
    ;; 2-byte sequence
    ((and (>= first-byte #xC0) (< first-byte #xE0))
     (let ((b2 (or (read-byte-from-fd fd) 0)))
       (code-char (logior (ash (logand first-byte #x1F) 6)
                          (logand b2 #x3F)))))
    ;; 3-byte sequence
    ((and (>= first-byte #xE0) (< first-byte #xF0))
     (let ((b2 (or (read-byte-from-fd fd) 0))
           (b3 (or (read-byte-from-fd fd) 0)))
       (code-char (logior (ash (logand first-byte #x0F) 12)
                          (ash (logand b2 #x3F) 6)
                          (logand b3 #x3F)))))
    ;; 4-byte sequence
    ((>= first-byte #xF0)
     (let ((b2 (or (read-byte-from-fd fd) 0))
           (b3 (or (read-byte-from-fd fd) 0))
           (b4 (or (read-byte-from-fd fd) 0)))
       (code-char (logior (ash (logand first-byte #x07) 18)
                          (ash (logand b2 #x3F) 12)
                          (ash (logand b3 #x3F) 6)
                          (logand b4 #x3F)))))
    (t (code-char first-byte))))

(defun parse-csi-sequence (fd)
  "Parse a CSI escape sequence after ESC ["
  (let ((first-byte (or (read-byte-from-fd fd)
                        (progn (sleep 0.001) (read-byte-from-fd fd)))))
    (unless first-byte
      (return-from parse-csi-sequence (make-key-event :code :unknown)))
    (cond
      ;; SGR mouse: ESC [ < ...
      ((= first-byte 60)
       (parse-sgr-mouse fd))
      ;; Normal CSI sequence
      (t
       (parse-normal-csi first-byte fd)))))

(defun parse-sgr-mouse (fd)
  "Parse SGR mouse sequence: ESC [ < Cb ; Cx ; Cy M/m"
  (let ((params nil)
        (final nil))
    (loop for attempts from 0 below 100 do
      (let ((b (read-byte-from-fd fd)))
        (cond
          (b (cond
               ((or (and (>= b 48) (<= b 57)) (= b 59))
                (push (code-char b) params))
               (t (setf final b) (return))))
          (t (sleep 0.001)))))
    (let* ((param-str (coerce (nreverse params) 'string))
           (parts (uiop:split-string param-str :separator '(#\;)))
           (cb (if (first parts) (parse-integer (first parts) :junk-allowed t) 0))
           (cx (if (second parts) (parse-integer (second parts) :junk-allowed t) 0))
           (cy (if (third parts) (parse-integer (third parts) :junk-allowed t) 0))
           (release-p (and final (= final 109))))
      (declare (ignore release-p))
      (cond
        ;; Scroll up
        ((= cb 64) (make-key-event :code +key-up+))
        ;; Scroll down
        ((= cb 65) (make-key-event :code +key-down+))
        ;; Click
        ((and final (= final 77))
         (make-key-event :code +key-mouse+ :mouse-x cx :mouse-y cy))
        (t nil)))))

(defun parse-normal-csi (first-byte fd)
  "Parse normal CSI sequence"
  (let ((params (list (code-char first-byte)))
        (final-byte nil))
    ;; If first-byte is already a final byte, use it
    (if (and (>= first-byte 64) (<= first-byte 126))
        (setf final-byte first-byte)
        ;; Otherwise continue reading
        (loop
          (let ((b (read-byte-from-fd fd)))
            (unless b (return))
            (cond
              ((and (>= b 48) (<= b 57))
               (push (code-char b) params))
              ((= b 59)
               (push #\; params))
              (t (setf final-byte b) (return))))))
    (let ((param-str (coerce (nreverse params) 'string)))
      (case final-byte
        (65 (make-key-event :code +key-up+))
        (66 (make-key-event :code +key-down+))
        (67 (make-key-event :code +key-right+))
        (68 (make-key-event :code +key-left+))
        (72 (make-key-event :code +key-home+))
        (70 (make-key-event :code +key-end+))
        (126
         (cond
           ((string= param-str "3") (make-key-event :code +key-delete+))
           ((string= param-str "5") (make-key-event :code +key-page-up+))
           ((string= param-str "6") (make-key-event :code +key-page-down+))
           (t (make-key-event :code :unknown))))
        (t (make-key-event :code :unknown))))))

;;; Global input reader instance
(defparameter *input-reader* (make-instance 'input-reader))

(defun close-tty-stream ()
  "Close the TTY stream"
  (reader-close *input-reader*))

(defun read-key ()
  "Read a key event from terminal. Polls until key pressed."
  (reader-open *input-reader*)
  (loop
    (let ((key (read-key-event *input-reader*)))
      (when key (return key)))
    (sleep 0.01)))

(defun read-key-with-timeout (timeout-ms)
  "Try to read a key event with timeout. Returns key-event or nil."
  (reader-open *input-reader*)
  (let ((start-time (get-internal-real-time))
        (timeout-ticks (* timeout-ms (/ internal-time-units-per-second 1000))))
    (loop
      (let ((key (read-key-event *input-reader*)))
        (when key (return key)))
      (when (> (- (get-internal-real-time) start-time) timeout-ticks)
        (return nil))
      (sleep 0.01))))
