(asdf:defsystem #:clatter
  :description "CLatter - WeeChat-style IRC TUI in Common Lisp (Pure ANSI version)"
  :author "Glenn Thompson"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:bordeaux-threads #:alexandria #:usocket #:cl+ssl #:flexi-streams #:cl-ppcre #:cl-base64 #:uiop)
  :serial t
  :components
  ((:file "src/package")
   ;; ANSI terminal layer
   (:file "src/ansi/ansi")
   (:file "src/ansi/terminal")
   ;; UI submit macro - loaded early so other modules can use it
   (:file "src/ansi/submit")
   ;; Core modules
   (:file "src/core/constants")
   (:file "src/core/debug")
   (:file "src/core/ring")
   (:file "src/core/model")
   (:file "src/core/config")
   (:file "src/core/protocol")
   (:file "src/core/events")
   (:file "src/core/logging")
   (:file "src/core/dispatch")
   (:file "src/core/handlers")
   ;; Network modules (use ui-submit macro)
   (:file "src/net/irc")
   (:file "src/net/dcc")
   ;; Commands (use ui-submit macro)
   (:file "src/core/commands")
   (:file "src/core/command-classes")
   ;; UI modules
   (:file "src/ui/input")
   (:file "src/ansi/theme")
   (:file "src/ansi/widgets")
   (:file "src/ansi/layout")
   (:file "src/ansi/tui")
   ;; Application
   (:file "src/app")
   (:file "src/main")))
