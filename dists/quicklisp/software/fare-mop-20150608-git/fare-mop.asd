;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem :fare-mop
  :version "1.0.1"
  :description "Utilities using the MOP; notably make informative pretty-printing trivial"
  :long-description "fare-mop is a small collection of utilities using the MOP.
It notably contains a method SIMPLE-PRINT-OBJECT, and
a mixin SIMPLE-PRINT-OBJECT-MIXIN that allow you to trivially define
PRINT-OBJECT methods that print the interesting slots in your objects,
which is great for REPL interaction and debugging."
  :author "Francois-Rene Rideau"
  :depends-on ((:version :fare-utils "1.0.0") #-asdf3 :uiop :closer-mop)
  :components
  ((:file "package")
   (:file "utilities" :depends-on ("package"))))
