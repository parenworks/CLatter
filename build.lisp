;;; build.lisp - Build script for CLatter executable
;;; Run with: sbcl --non-interactive --load build.lisp

(require :asdf)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first:
  curl -O https://beta.quicklisp.org/quicklisp.lisp
  sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit")))

;; Add project to ASDF registry
(push *default-pathname-defaults* asdf:*central-registry*)

;; Load the system (ql:quickload will fetch missing dependencies)
(ql:quickload :clatter)

;; Build the executable
(sb-ext:save-lisp-and-die "clatter"
                          :toplevel #'clatter:main
                          :executable t
                          :compression t
                          :save-runtime-options nil)
