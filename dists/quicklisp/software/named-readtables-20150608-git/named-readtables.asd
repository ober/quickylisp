;;;; -*- mode: Lisp -*-

(in-package :cl-user)

(defclass asdf::named-readtables-source-file (asdf:cl-source-file) ())

#+sbcl
(defmethod asdf:perform :around ((o asdf:compile-op)
                                 (c asdf::named-readtables-source-file))
  (let ((sb-ext:*derive-function-types* t))
    (call-next-method)))

(asdf:defsystem :named-readtables
  :description "Library that creates a namespace for named readtable
  akin to the namespace of packages."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :maintainer "Gábor Melis"
  :mailto "mega@retes.hu"
  :version "0.9"
  :licence "BSD, see LICENSE"
  :default-component-class asdf::named-readtables-source-file
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "define-api")
                             (:file "cruft")
                             (:file "named-readtables")))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :named-readtables))))
  (asdf:operate 'asdf:load-op :named-readtables-test)
  (asdf:operate 'asdf:test-op :named-readtables-test))

(asdf:defsystem :named-readtables-test
  :description "Test suite for the Named-Readtables library."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :maintainer "Gábor Melis"
  :mailto "mega@retes.hu"
  :depends-on (:named-readtables)
  :components
  ((:module "test"
            :default-component-class asdf::named-readtables-source-file
            :serial t
            :components ((:file "package")
                         (:file "rt")
                         (:file "tests")))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system
                                  :named-readtables-test))))
  (let ((*package* (find-package :named-readtables-test)))
    (funcall (intern (string '#:do-tests) *package*))))

;;; MGL-PAX depends on NAMED-READTABLES so we must put documentation
;;; in a separate system in order to be able to use MGL-PAX.
(asdf:defsystem :named-readtables-doc
  :depends-on (:named-readtables :mgl-pax)
  :components
  ((:module "src"
    :serial t
    :components ((:file "doc")))))
