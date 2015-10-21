(in-package :cl-user)
(defpackage jonathan-test-asd
  (:use :cl :asdf))
(in-package :jonathan-test-asd)

(defsystem jonathan-test
  :author "Rudolph-Miller"
  :license "MIT"
  :description "Tests of Jonathan."
  :depends-on (:jonathan
               :prove)
  :components ((:module "t"
                :components
                ((:file "jonathan")
                 (:test-file "util")
                 (:test-file "encode")
                 (:test-file "decode")
                 (:test-file "helper"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                      (unwind-protect
                           (funcall (intern #.(string :run-test-system) :prove.asdf) c))))
