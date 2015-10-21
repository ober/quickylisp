;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#.(unless (or #+asdf3 (asdf/driver:version<= "2.32" (asdf-version)))
    (error "You need ASDF >= 2.32 to load this system correctly."))

(defsystem :swap-bytes
  :author "Stas Boukarev <stassats@gmail.com>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :description "Optimized byte-swapping primitives"
  :version (:read-file-form "version.sexp")
  :licence "MIT"
  :defsystem-depends-on (:trivial-features)
  :depends-on (:trivial-features)
  :components ((:file "package")
               (:file "ccl"
                :if-feature (:and :ccl (:or :x86 :x86-64))
                :depends-on ("package"))
               (:file "sbcl-defknowns"
                :if-feature (:and :sbcl (:or :x86 :x86-64))
                :depends-on ("package"))
               (:file "sbcl-vops"
                :if-feature (:and :sbcl (:or :x86 :x86-64))
                :depends-on ("package" "sbcl-defknowns"))
               (:file "sbcl"
                :if-feature (:and :sbcl (:or :x86 :x86-64))
                :depends-on ("package" "sbcl-defknowns" "sbcl-vops"))
               (:file "portable"
                :if-feature (:not (:or (:and :ccl (:or :x86 :x86-64))
                                       (:and :sbcl (:or :x86 :x86-64))))
                :depends-on ("package" "ccl" "sbcl"))
               (:file "network" :depends-on ("package" "portable"))
               (:file "endianness" :depends-on ("package" "portable"))))

(defsystem :swap-bytes/test
  :depends-on (:swap-bytes :fiveam)
  :components ((:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :swap-bytes))))
  (load-system :swap-bytes/test :force '(:swap-bytes/test))
  (uiop:symbol-call :5am :run! :swap-bytes))
