;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; optima implementation for fare-quasiquote
;;; Copyright (c) 2002-2014 Fahree Wreido <fare@tunes.org>

#+xcvb (module (:depends-on ("quasiquote" (:asdf "optima"))))

(in-package :fare-quasiquote)

(eval-when (:compile-toplevel :load-toplevel :execute)

;; the below instruction enables pattern-matching for the simplifier.
(macrolet ((def (&rest x)
               `(progn
                  ,@(loop :for (a b) :on x :by #'cddr :collect
                          `(optima:defpattern ,a (&rest r) `(,',b ,@r))))))
  (def
    list  cl:list
    list* cl:list*
    cons  cl:cons
    quote cl:quote))

(optima:defpattern quasiquote (x)
  (quasiquote-expand x))

(defstruct (quasiquote-vector-pattern (:include optima::constructor-pattern)
                                      (:constructor make-quasiquote-vector-pattern
                                          (content-pattern
                                           &aux (optima::subpatterns (list content-pattern))))))

(defmethod optima::constructor-pattern-destructor-sharable-p
    ((x quasiquote-vector-pattern) (y quasiquote-vector-pattern))
  t)

(defmethod optima::constructor-pattern-make-destructor ((pattern quasiquote-vector-pattern) var)
  (optima::make-destructor
   :predicate-form `(simple-vector-p ,var)
   :accessor-forms (list `(coerce ,var 'cl:list))))

(defmethod optima::parse-constructor-pattern ((name (eql 'make-vector)) &rest args)
  (optima:match args
    ((cl:list (cl:cons (cl:quote list) pats))
     (apply #'optima::make-vector-pattern (mapcar #'optima::parse-pattern pats)))
    ((cl:list pat)
     (make-quasiquote-vector-pattern (optima::parse-pattern pat)))
    (otherwise
     (error "Bad make-vector pattern: ~S" (cons 'make-vector args)))))

(defmethod optima::unparse-pattern ((pattern quasiquote-vector-pattern))
  `(make-vector ,(optima::unparse-pattern (first (optima::constructor-pattern-subpatterns pattern)))))

);eval-when
