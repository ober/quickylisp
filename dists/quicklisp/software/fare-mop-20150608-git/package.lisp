;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module nil)

(in-package #:cl-user)

(defpackage #:fare-mop
  (:use #:fare-utils #:closer-common-lisp :asdf/driver)
  (:export
   #:simple-print-object
   #:simple-print-object-mixin
   #:slots-to-print
   #:collect-slots
   #:remake-object
   ))
