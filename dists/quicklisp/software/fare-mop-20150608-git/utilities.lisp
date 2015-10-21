#+xcvb (module (:depends-on ("package")))

(in-package :fare-mop)

;;; CLOS magic (depends on closer-mop) (from philip-jose)

(defun collect-slots (object &key (slots t))
  "Given an OBJECT and SLOTS designating a list of slot designators,
return a p-list of an INITARG and VALUE
for each of those of the specified slots that are bound.
A slot designator may be a SLOT-DEFINITION object,
a KEYWORD specifying the INITARG of the slot,
or a SYMBOL naming the slot.
If the slot has no INITARG, the slot name is used instead in the p-list"
  (let* ((class (class-of object))
         (slot-cache t))
    (flet ((all-slots ()
             (unless (listp slot-cache)
               (setf slot-cache (compute-slots class)))
             slot-cache)
           (initarg (slotd)
             (or (first (slot-definition-initargs slotd))
                 (slot-definition-name slotd))))
     (loop :for slot :in (if (eq slots t) (all-slots) slots) :nconc
       (multiple-value-bind (name initarg)
           (etypecase slot
             (slot-definition
              (values (slot-definition-name slot) (initarg slot)))
             (keyword
              (let ((slotd (find-if
                           #'(lambda (s)
                               (member slot (slot-definition-initargs s)))
                           (all-slots))))
                (values (slot-definition-name slotd) slot)))
             (symbol
              (let ((slotd (find slot (all-slots)
                                 :key #'slot-definition-name)))
                (values slot (initarg slotd)))))
         (when (slot-boundp object name)
           (list initarg (slot-value object name))))))))

(defun simple-print-object (object stream &key identity (slots t))
  "This function will introspect you object's slots to print it.
You can call it explicitly from your print-object methods,
or implicitly by just inheritting from SIMPLE-PRINT-OBJECT-MIXIN.
If SLOTS is T (the default), print all slots as per CLOSER-MOP:COMPUTE-SLOTS.
If SLOTS is a list of slot designators, print them.
Slots are printed as a p-list as per FARE-MOP:COLLECT-SLOTS.
The IDENTITY keyword is as per PRINT-UNREADABLE-OBJECT."
  (with-output (stream)
    (print-unreadable-object (object stream :type t :identity identity)
      (write (collect-slots object :slots slots) :stream stream))))

(defgeneric slots-to-print (object)
  (:method ((object t))
    t))

(defclass simple-print-object-mixin (standard-object)
  ()
  (:documentation "Mixin to trivially give a print-object method to your class.
If you don't want to print all slots, define a method on FARE-MOP:SLOTS-TO-PRINT
to return a list of slot designators for those slots you want to print,
as per FARE-MOP:COLLECT-SLOTS."))

(defmethod print-object ((object simple-print-object-mixin) stream)
  (simple-print-object object stream :slots (slots-to-print object)))

(defun remake-object (object &rest keys &key &allow-other-keys)
  "A function to build a new object of the same class as OBJECT,
initialized with the specified KEYS, plus keys deduced from the original object
by those slots that both are bound and have a defined initarg.
Note: this function is useful for experimental programming,
but use of runtime introspection means
this function isn't suited where performance is important."
  (loop :with class = (class-of object)
        :with slots = (compute-slots class)
        :for slot :in slots
        :for name = (slot-definition-name slot)
        :for initarg = (first (slot-definition-initargs slot))
        :when (and initarg (slot-boundp object name))
        :nconc `(,initarg ,(slot-value object name)) :into old-keys
        :finally (return (apply #'make-instance class (append keys old-keys)))))
