(in-package :cl-user)
(defpackage prove-asd
  (:use :cl :asdf))
(in-package :prove-asd)

(defsystem prove
  :version "1.0.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:cl-ppcre
               #-abcl :cl-ansi-text
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "prove" :depends-on ("output" "test" "suite" "asdf" "color"))
                 (:file "test" :depends-on ("output" "report" "reporter" "suite"))
                 (:file "report")
                 (:file "reporter" :depends-on ("report" "output"))
                 (:module "reporter-components"
                  :pathname "reporter"
                  :depends-on ("report" "reporter" "color")
                  :components
                  ((:file "tap")
                   (:file "fiveam")
                   (:file "list")
                   (:file "dot" :depends-on ("list"))))
                 (:file "suite" :depends-on ("output" "report" "reporter" "asdf"))
                 (:file "asdf" :depends-on ("output"))
                 (:file "color")
                 (:file "output")))))
