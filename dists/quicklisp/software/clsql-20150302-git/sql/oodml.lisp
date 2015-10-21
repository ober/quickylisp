;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; The CLSQL Object Oriented Data Manipulation Language (OODML).
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defun find-normalized-key (obj)
  "Find the first / primary key of a normalized object"
  (find-slot-if obj #'key-slot-p T T))

(defun normalized-key-value (obj)
  "Normalized classes share a single key for all their key slots"
  (when (normalizedp (class-of obj))
    (easy-slot-value obj (find-normalized-key obj))))

(defun key-qualifier-for-instance (obj &key (database *default-database*) this-class)
  "Generate a boolean sql-expression that identifies an object by its keys"
  (let* ((obj-class (or this-class (class-of obj)))
         (keys (keyslots-for-class obj-class))
         (normal-db-value (normalized-key-value obj)))
    (when keys
      (labels ((db-value (k)
                 (or normal-db-value
                     (db-value-from-slot
                      k
                      (easy-slot-value obj k)
                      database)))
               (key-equal-exp (k)
                 (sql-operation '== (generate-attribute-reference obj-class k database)
                                (db-value k))))
        (clsql-ands (mapcar #'key-equal-exp keys))))))

(defun generate-attribute-reference (vclass slotdef &optional (database *default-database*))
  "Turns key class and slot-def into a sql-expression representing the
   table and column it comes from

   used by things like make-select-list, update-slot-from-record"
  (when (key-or-base-slot-p slotdef)
    (sql-expression :attribute (database-identifier slotdef database)
                    :table (database-identifier vclass database))))

(defun get-join-slots (class &optional retrieval-method)
  "Returns list of join slots for a class.

   if a retrieval method is specified only return slots of that type
   if the retrieval method is T, nil or :all return all join slots"
  (assert (member retrieval-method '(nil t :all :immediate :deferred)))
  (setf class (to-class class))
  (let ((all? (member retrieval-method '(nil t :all))))
    (loop for slot in (ordered-class-slots class)
          when (and (join-slot-p slot)
                    (or all? (eql (join-slot-retrieval-method slot) retrieval-method)))
          collect slot)))

(defun immediate-join-slots (class)
  (get-join-slots class :immediate))

(defmethod choose-database-for-instance ((obj standard-db-object) &optional database)
  "Determine which database connection to use for a standard-db-object.
        Errs if none is available."
  (or (find-if #'(lambda (db)
                   (and db (is-database-open db)))
               (list (view-database obj)
                     database
                     *default-database*))
      (signal-no-database-error nil)))



(defmethod update-slot-with-null ((object standard-db-object) slotdef)
  "sets a slot to the void value of the slot-def (usually nil)"
  (setf (easy-slot-value object slotdef)
        (slot-value slotdef 'void-value)))

(defmethod update-slot-from-db-value ((instance standard-db-object) slotdef value)
  "This gets a value from the database and turns it itno a lisp value
   based on the slot's slot-db-reader or baring that read-sql-value"
  (declare (optimize (speed 3) #+cmu (extensions:inhibit-warnings 3)))
  (let* ((slot-reader (view-class-slot-db-reader slotdef))
         (slot-type   (specified-type slotdef)))
    (cond
      ((null value) (update-slot-with-null instance slotdef))
      ((null slot-reader)
       (setf (easy-slot-value instance slotdef)
             (read-sql-value value (delistify slot-type)
                             (choose-database-for-instance instance)
                             (database-underlying-type
                              (choose-database-for-instance instance)))))
      (t (etypecase slot-reader
           ((or symbol function)
            (setf (easy-slot-value instance slotdef)
                  (apply slot-reader (list value))))
           (string
            (setf (easy-slot-value instance slotdef)
                  (format nil slot-reader value))))))))

(defmethod key-value-from-db (slotdef value database)
  "TODO: is this deprecated? there are no uses anywhere in clsql"
  (declare (optimize (speed 3) #+cmu (extensions:inhibit-warnings 3)))
  (let ((slot-reader (view-class-slot-db-reader slotdef))
        (slot-type (specified-type slotdef)))
    (cond ((and value (null slot-reader))
           (read-sql-value value (delistify slot-type) database
                           (database-underlying-type database)))
          ((null value)
           nil)
          ((typep slot-reader 'string)
           (format nil slot-reader value))
          ((typep slot-reader '(or symbol function))
           (apply slot-reader (list value)))
          (t
           (error "Slot reader is of an unusual type.")))))

(defun db-value-from-slot (slotdef val database)
  (let ((dbwriter (view-class-slot-db-writer slotdef))
        (dbtype (specified-type slotdef)))
    (typecase dbwriter
      (string (format nil dbwriter val))
      ((and (or symbol function) (not null)) (apply dbwriter (list val)))
      (t
       (database-output-sql-as-type
        (typecase dbtype
          (cons (car dbtype))
          (t dbtype))
        val database (database-underlying-type database))))))

(defun check-slot-type (slotdef val)
  (let* ((slot-type (specified-type slotdef))
         (basetype (if (listp slot-type) (car slot-type) slot-type)))
    (when (and slot-type val)
      (unless (typep val basetype)
        (error 'sql-user-error
               :message
               (format nil "Invalid value ~A in slot ~A, not of type ~A."
                       val (slot-definition-name slotdef) slot-type))))))

(defmethod get-slot-values-from-view (obj slotdeflist values)
  "Used to copy values from the database into the object
   used by things like find-all and select"
  (loop for slot in slotdeflist
        for value in values
        do (update-slot-from-db-value obj slot value))
  obj)

(defclass class-and-slots ()
  ((view-class :accessor view-class :initarg :view-class :initform nil)
   (slot-defs :accessor slot-defs :initarg :slot-defs :initform nil))
  (:documentation "A helper class to keep track of which slot-defs from a
   table need to be updated, a normalized class might have many of these
   because each of its parent classes might represent some other table and we
   need to match which slots came from which parent class/table"))

(defun make-class-and-slots (c &optional s)
  "Create a new class-and-slots object"
  (make-instance 'class-and-slots :view-class c :slot-defs (listify s) ))

(defmethod view-table ((o class-and-slots))
  "get the view-table of the view-class of o"
  (view-table (view-class o)))

(defmethod view-table-exp ((o class-and-slots))
  (sql-expression :table (view-table o)))

(defmethod view-table-exp ((o standard-db-class))
  (sql-expression :table (view-table o)))

(defmethod attribute-references ((o class-and-slots))
  "build sql-ident-attributes for a given class-and-slots"
  (loop
    with class = (view-class o)
    for sd in (slot-defs o)
    collect (generate-attribute-reference class sd)))

(defmethod attribute-value-pairs ((def class-and-slots) (o standard-db-object)
                                  database)
  "for a given class-and-slots and object, create the sql-expression & value pairs
   that need to be sent to the database"
  (loop for s in (slot-defs def)
        for n = (to-slot-name s)
        when (slot-boundp o n)
        collect (make-attribute-value-pair s (slot-value o n) database)))

(defmethod view-classes-and-slots-by-name ((obj standard-db-object) slots-to-match)
  "If it's normalized, find the class that actually contains
   the slot that's tied to the db,

   otherwise just search the current class
  "
  (let* ((view-class (class-of obj))
         (normalizedp (normalizedp view-class))
         rtns)
    (labels ((get-c&s-obj (class)
               (or (find class rtns :key #'view-class)
                   (first (push (make-class-and-slots class) rtns))))
             (associate-slot-with-class (class slot)
               "Find the best class to associate with the slot. If it is
                normalized then it needs to be a direct slot otherwise it just
                needs to be on the class."
               (let ((sd (find-slot-by-name class slot normalizedp nil)))
                 (if sd
                     ;;we found it directly or it's (not normalized)
                     (pushnew sd (slot-defs (get-c&s-obj class)))
                     (when normalizedp
                       (loop for parent in (class-direct-superclasses class)
                             until (associate-slot-with-class parent slot))))
                 sd)))
      (loop
        for in-slot in (listify slots-to-match)
        do (associate-slot-with-class view-class in-slot)))
    rtns))

(defun update-auto-increments-keys (class obj database)
  " handle pulling any autoincrement values into the object
    Also handles normalized key chaining"
  (let ((pk-slots (keyslots-for-class class))
        (table (view-table class))
        new-pk-value)
    (labels ((do-update (slot &aux (val (easy-slot-value obj slot)))
               (if val
                   (setf new-pk-value val)
                   (update-slot-from-db-value
                    obj slot
                    (or new-pk-value
                        (setf new-pk-value
                              (database-last-auto-increment-id
                               database table slot))))))
             ;; NB: This interacts very strangely with autoincrement keys
             ;; (see changelog 2014-01-30)
             (chain-primary-keys (in-class)
               "This seems kindof wrong, but this is mostly how it was working, so
                  its here to keep the normalized code path working"
               (when (typep in-class 'standard-db-class)
                 (loop for slot in (ordered-class-slots in-class)
                       when (key-slot-p slot)
                       do (do-update slot)))))
      (loop for slot in pk-slots do (do-update slot))
      (let ((direct-class (to-class obj)))
        (when (and new-pk-value (normalizedp direct-class))
          (chain-primary-keys direct-class)))
      new-pk-value)))

(defmethod %update-instance-helper
    (class-and-slots obj database
     &aux (avps (attribute-value-pairs class-and-slots obj database)))
  "A function to help us update a given table (based on class-and-slots)
   with values from an object"
  ;; we dont actually need to update anything on this particular
  ;; class / parent class
  (unless avps (return-from %update-instance-helper))

  (let* ((view-class (view-class class-and-slots))
         (table (view-table view-class))
         (table-sql (sql-expression :table table)))

    ;; view database is the flag we use to tell it was pulled from a database
    ;; and thus probably needs an update instead of an insert
    (cond ((view-database obj)
           (let ((where (key-qualifier-for-instance
                         obj :database database :this-class view-class)))
             (unless where
               (error "update-record-from-*: could not generate a where clause for ~a using ~A"
                      obj view-class))
             (update-records table-sql
                             :av-pairs avps
                             :where where
                             :database database)))
          (T ;; was not pulled from the db so insert it
           ;; avps MUST contain any primary key slots set
           ;; by previous inserts of the same object into different
           ;; tables (ie: normalized stuff)
           (insert-records :into table-sql
                           :av-pairs avps
                           :database database)
           ;; also handles normalized-class key chaining
           (update-auto-increments-keys view-class obj database)
           ;; we dont set view database here, because there could be
           ;; N of these for each call to update-record-from-* because
           ;; of normalized classes
           ))
    (update-slot-default-values obj class-and-slots)))

(defmethod update-record-from-slots ((obj standard-db-object) slots
                                     &key (database *default-database*))
  "For a given list of slots, update all records associated with those slots
   and classes.

   Generally this will update the single record associated with this object,
   but for normalized classes might update as many records as there are
   inheritances "
  (setf slots (listify slots))
  (let* ((classes-and-slots (view-classes-and-slots-by-name obj slots))
         (database (choose-database-for-instance obj database)))
    (loop for class-and-slots in classes-and-slots
          do (%update-instance-helper class-and-slots obj database))
    (setf (slot-value obj 'view-database) database))
  (values))

(defmethod update-record-from-slot
    ((obj standard-db-object) slot &key (database *default-database*))
  "just call update-records-from-slots which now handles this.

   This function is only here to maintain backwards compatibility in
   the public api"
  (update-record-from-slots obj slot :database database))

(defmethod view-classes-and-storable-slots (class &key to-database-p)
  "Get a list of all the tables we need to update and the slots on them

   for non normalized classes we return the class and all its storable slots

   for normalized classes we return a list of direct slots and the class they
   came from for each normalized view class

   to-database-p is provided so that we can read / write different data
   to the database in different circumstances
   (specifically clsql-helper:dirty-db-slots-mixin which only updates slots
    that have changed )
  "
  (setf class (to-class class))
  (let* (rtns)
    (labels ((storable-slots (class)
               (loop for sd in (slots-for-possibly-normalized-class class)
                     when (and (key-or-base-slot-p sd)
                               ;; we dont want to insert/update auto-increments
                               ;; but we do read them
                               (not (and to-database-p (auto-increment-column-p sd))))
                     collect sd))
             (get-classes-and-slots (class &aux (normalizedp (normalizedp class)))
               (let ((slots (storable-slots class)))
                 (when slots
                   (push (make-class-and-slots class slots) rtns)))
               (when normalizedp
                 (loop for new-class in (class-direct-superclasses class)
                       do (when (typep new-class 'standard-db-class)
                            (get-classes-and-slots new-class))))))
      (get-classes-and-slots class))
    rtns))

(defmethod primary-key-slot-values ((obj standard-db-object)
                                    &key class slots )
  "Returns the values of all key-slots for a given class"
  (defaulting class (class-of obj)
              slots (keyslots-for-class class))
  (loop for slot in slots
        collect (easy-slot-value obj slot)))

(defmethod update-slot-default-values ((obj standard-db-object)
                                       classes-and-slots)
  "Makes sure that if a class has unfilled slots that claim to have a default,
   that we retrieve those defaults from the database

   TODO: use update-slots-from-record (doesnt exist) instead to batch this!"
  (loop for class-and-slots in (listify classes-and-slots)
        do (loop for slot in (slot-defs class-and-slots)
                 do (when (and (slot-has-default-p slot)
                               (not (easy-slot-value obj slot)))
                      (update-slot-from-record obj (to-slot-name slot))))))

(defmethod update-records-from-instance ((obj standard-db-object)
                                         &key (database *default-database*))
  "Updates the records in the database associated with this object if
   view-database slot on the object is nil then the object is assumed to be
   new and is inserted"
  (let ((database (choose-database-for-instance obj database))
        (classes-and-slots (view-classes-and-storable-slots obj :to-database-p t)))
    (loop for class-and-slots in classes-and-slots
          do (%update-instance-helper class-and-slots obj database))
    (setf (slot-value obj 'view-database) database)
    (primary-key-slot-values obj)))

(defmethod delete-instance-records ((instance standard-db-object) &key database)
  "Removes the records associated with a given instance
   (as determined by key-qualifier-for-instance)

   TODO: Doesnt handle normalized classes at all afaict"
  (let ((database (choose-database-for-instance instance database))
        (vt (sql-expression :table (view-table (class-of instance)))))
    (if database
        (let ((qualifier (key-qualifier-for-instance instance :database database)))
          (delete-records :from vt :where qualifier :database database)
          (setf (record-caches database) nil)
          (setf (slot-value instance 'view-database) nil)
          (values))
        (signal-no-database-error database))))

(defmethod update-instance-from-records ((instance standard-db-object)
                                         &key (database *default-database*))
  "Updates a database object with the current values stored in the database

   TODO: Should this update immediate join slots similar to build-objects?
         Can we just call build-objects?, update-objects-joins?
  "

  (let* ((classes-and-slots (view-classes-and-storable-slots
                             instance :to-database-p nil))
         (vd (choose-database-for-instance instance database)))
    (labels ((do-update (class-and-slots)
               (let* ((select-list (make-select-list class-and-slots
                                                     :do-joins-p nil
                                                     :database database))
                      (view-table (sql-table select-list))
                      (view-qual (key-qualifier-for-instance
                                  instance :database vd
                                  :this-class (view-class select-list)))
                      (res (when view-qual
                             (first
                              (apply #'select
                                     (append (full-select-list select-list)
                                             (list :from view-table
                                                   :where view-qual
                                                   :result-types nil
                                                   :database vd)))))))
                 (when res
                   (setf (slot-value instance 'view-database) vd)
                   (get-slot-values-from-view instance (slot-list select-list) res))
                 )))
      (loop for class-and-slots in classes-and-slots
            do (do-update class-and-slots)))))


(defmethod get-slot-value-from-record ((instance standard-db-object)
                                       slot &key (database *default-database*))
  (let* ((class-and-slot
           (first
            (view-classes-and-slots-by-name instance slot)))
         (view-class (view-class class-and-slot))
         (slot-def (first (slot-defs class-and-slot)))
         (vd (choose-database-for-instance instance database))
         (att-ref (first (attribute-references class-and-slot)))
         (res (first
               (select att-ref
                 :from (view-table-exp class-and-slot)
                 :where (key-qualifier-for-instance
                         instance
                         :database vd
                         :this-class view-class)
                 :result-types nil
                 :flatp T))))
    (values res slot-def)))

(defmethod update-slot-from-record ((instance standard-db-object)
                                    slot &key (database *default-database*))
  "Pulls the value of a given slot form the database and stores that in the
   appropriate slot on instance"
  (multiple-value-bind (res slot-def)
      (get-slot-value-from-record instance slot :database database)
    (let ((vd (choose-database-for-instance instance database)))
      (setf (slot-value instance 'view-database) vd)
      (update-slot-from-db-value instance slot-def res))))


(defvar +no-slot-value+ '+no-slot-value+)

(defsql sql-slot-value (:symbol "slot-value") (classname slot &optional (value +no-slot-value+) (database *default-database*))
        (let* ((class (find-class classname))
               (sld (slotdef-for-slot-with-class slot class)))
          (if sld
              (if (eq value +no-slot-value+)
                  (sql-expression :attribute (database-identifier sld database)
                                  :table (view-table class))
                  (db-value-from-slot
                   sld
                   value
                   database))
              (error "Unknown slot ~A for class ~A" slot classname))))

(defsql sql-view-class (:symbol "view-class") (classname &optional (database *default-database*))
        (declare (ignore database))
        (let* ((class (find-class classname)))
          (unless (view-table class)
            (error "No view-table for class ~A"  classname))
          (sql-expression :table (view-table class))))

(deftype tinyint ()
  "An 8-bit integer, this width may vary by SQL implementation."
  'integer)

(deftype smallint ()
  "An integer smaller than a 32-bit integer. this width may vary by SQL implementation."
  'integer)

(deftype mediumint ()
  "An integer smaller than a 32-bit integer, but may be larger than a smallint. This width may vary by SQL implementation."
  'integer)

(deftype bigint ()
  "An integer larger than a 32-bit integer, this width may vary by SQL implementation."
  'integer)

(deftype varchar (&optional size)
  "A variable length string for the SQL varchar type."
  (declare (ignore size))
  'string)

(deftype universal-time ()
  "A positive integer as returned by GET-UNIVERSAL-TIME."
  '(integer 1 *))

(deftype generalized-boolean ()
  "A type which outputs a SQL boolean value, though any lisp type can be stored in the slot."
  t)

#+ignore
(deftype char (&optional len)
  "A lisp type for the SQL CHAR type."
  `(string ,len))

(defmethod database-get-type-specifier ((type string) args database (db-type t))
  "Pass through the literal type as defined in the type string"
  (declare (ignore args database db-type))
  type)


(defmethod database-get-type-specifier ((type symbol) args database db-type)
  (case type
    (char (if args
              (format nil "CHAR(~D)" (first args))
              "CHAR(1)"))
    ((varchar string symbol keyword)
     (if args
         (format nil "VARCHAR(~A)" (car args))
         (format nil "VARCHAR(~D)" *default-string-length*)))
    ((longchar text) "text")
    (integer (if args
                 (format nil "INT(~A)" (car args))
                 "INT"))
    ((tinyint smallint mediumint) "INT")
    ((long-float float)
     (if args
         (format nil "FLOAT(~A)" (car args))
         "FLOAT"))
    ((bigint universal-time) "BIGINT")
    (number
     (cond
       ((and (consp args) (= (length args) 2))
        (format nil "NUMBER(~D,~D)" (first args) (second args)))
       ((and (consp args) (= (length args) 1))
        (format nil "NUMBER(~D)" (first args)))
       (t
        "NUMBER")))
    (wall-time "TIMESTAMP")
    (date "DATE")
    (duration "VARCHAR")
    (money "INT8")
    ((boolean generalized-boolean) "BOOL")
    (t (warn "Could not determine a valid ~A type specifier for ~A ~A ~A, defaulting to VARCHAR "
             db-type type args database)
     (format nil "VARCHAR(~D)" *default-string-length*))))

(defun print-readable-symbol (in &aux (*package* (find-package :keyword))
                                 (*print-readably* t))
  (prin1-to-string in))

(defmethod database-output-sql-as-type
    (type val database db-type
     &aux
     (*print-circle* t) (*print-array* t)
     (*print-length* nil) (*print-base* #10r10))
  (declare (ignore database))
  (cond 
    ((null type) val)
    ((member type '(boolean generalized-boolean))
     ;; booleans handle null differently
     (case db-type
       ;; done here so it can be done once
       ((:mssql :mysql) (if val 1 0))
       (otherwise (if val "t" "f"))))
    ((null val)
     (when (next-method-p)
       (call-next-method)))
    (t
     (case type
       ((or symbol keyword)
        (print-readable-symbol val))
       (string val)
       (char (etypecase val
               (character (write-to-string val))
               (string val)))
       (float (format nil "~F" val))
       ((list vector array)
        (prin1-to-string val))
       (otherwise
        (if (next-method-p)
            (call-next-method)
            val))))))


(defmethod read-sql-value :around
    (val type database db-type
     ;; never eval while reading values, always read base 10
     &aux *read-eval* (*read-base* #10r10))
  (declare (ignore db-type))
  (cond
    ;; null value or type
    ((or (equalp "nil" val) (eql 'null val)) nil) 
    
    ;; no specified type or already the right type
    ((or (null type)
         (ignore-errors (typep val type)))
     val)

    ;; actually convert
    (t 
     (let ((res (handler-bind
                    ;; all errors should be converted to sql-value-conversion-error
                    ((error (lambda (c)
                              (when *debugger-hook*
                                (invoke-debugger c))
                              (unless (typep c 'sql-value-conversion-error)
                                (error-converting-value val type database)))))
                  (call-next-method))))
       ;; if we didnt get the right type after converting, we should probably
       ;; error right away
       (maybe-error-converting-value
        res val type database)))))

(defmethod read-sql-value (val type database db-type)
  ;; errors, nulls and preconverted types are already handled in around
  (typecase type
    (symbol
     (case type
       ((string varchar) val)
       (char (string (schar val 0)))
       ((or keyword symbol)
        (read-from-string val))
       ((smallint mediumint bigint integer universal-time)
        (parse-integer val))
       ((double-float float)
        ;; ensure that whatever we got is coerced to a float of the correct
        ;; type (eg: 1=>1.0d0)
        (float
         (etypecase val
           (string (let ((*read-default-float-format*
                           (ecase type
                             (float 'single-float)
                             (double-float 'double-float))))
                     (read-from-string val)))
           ;; maybe wrong type of float
           (float val)) 
         (if (eql type 'double-float) 1.0d0 1.0s0)))
       (number (read-from-string val))
       ((boolean generalized-boolean)
        (if (member val '(nil t))
            val
            (etypecase val
              (string
               (when (member val '("1" "t" "true" "y") :test #'string-equal)
                 t))
              (number (not (zerop val))))))
       ((wall-time duration) (parse-timestring val))
       (date (parse-datestring val))
       (t (call-next-method))))
    (t (typecase val
         (string (read-from-string val))
         (t (error-converting-value val type database))))))

;; ------------------------------------------------------------
;; Logic for 'faulting in' :join slots

;; this works, but is inefficient requiring (+ 1 n-rows)
;; SQL queries
#+ignore
(defun fault-join-target-slot (class object slot-def)
  (let* ((res (fault-join-slot-raw class object slot-def))
         (dbi (view-class-slot-db-info slot-def))
         (target-name (gethash :target-slot dbi))
         (target-class (find-class target-name)))
    (when res
      (mapcar (lambda (obj)
                (list
                 (car
                  (fault-join-slot-raw
                   target-class
                   obj
                   (find target-name (class-slots (class-of obj))
                         :key #'slot-definition-name)))
                 obj))
              res)
      #+ignore ;; this doesn't work when attempting to call slot-value
      (mapcar (lambda (obj)
                (cons obj (slot-value obj ts))) res))))

(defun fault-join-target-slot (class object slot-def)
  (let* ((dbi (view-class-slot-db-info slot-def))
         (ts (gethash :target-slot dbi))
         (jc  (gethash :join-class dbi))
         (jc-view-table (view-table (find-class jc)))
         (tdbi (view-class-slot-db-info
                (find ts (class-slots (find-class jc))
                      :key #'slot-definition-name)))
         (retrieval (gethash :retrieval tdbi))
         (tsc (gethash :join-class tdbi))
         (ts-view-table (view-table (find-class tsc)))
         (jq (join-qualifier class object slot-def))
         (key (slot-value object (gethash :home-key dbi))))

    (when jq
      (ecase retrieval
        (:immediate
         (let ((res
                (find-all (list tsc)
                          :inner-join (sql-expression :table jc-view-table)
                          :on (sql-operation
                               '==
                               (sql-expression
                                :attribute (gethash :foreign-key tdbi)
                                :table ts-view-table)
                               (sql-expression
                                :attribute (gethash :home-key tdbi)
                                :table jc-view-table))
                          :where jq
                          :result-types :auto
                          :database (choose-database-for-instance object))))
           (mapcar #'(lambda (i)
                       (let* ((instance (car i))
                              (jcc (make-instance jc :view-database (choose-database-for-instance instance))))
                         (setf (slot-value jcc (gethash :foreign-key dbi))
                               key)
                         (setf (slot-value jcc (gethash :home-key tdbi))
                               (slot-value instance (gethash :foreign-key tdbi)))
                         (list instance jcc)))
                   res)))
        (:deferred
         ;; just fill in minimal slots
         (mapcar
          #'(lambda (k)
              (let ((instance (make-instance tsc :view-database (choose-database-for-instance object)))
                    (jcc (make-instance jc :view-database (choose-database-for-instance object)))
                    (fk (car k)))
                (setf (slot-value instance (gethash :home-key tdbi)) fk)
                (setf (slot-value jcc (gethash :foreign-key dbi))
                      key)
                (setf (slot-value jcc (gethash :home-key tdbi))
                      fk)
                (list instance jcc)))
          (select (sql-expression :attribute (gethash :foreign-key tdbi) :table jc-view-table)
                  :from (sql-expression :table jc-view-table)
                  :where jq
                  :database (choose-database-for-instance object))))))))


;;; Remote Joins

(defvar *default-update-objects-max-len* nil
  "The default value to use for the MAX-LEN keyword argument to
  UPDATE-OBJECT-JOINS.")

(defun %update-objects-joins-slot-defs (class slot-names)
  "Get the slot definitions for the joins slots specified as slot-names
   if slot-names is :immediate, :deferred or (or :all t) return all of
   that type of slot definitions"
  (setf class (to-class class))
  (when (eq t slot-names) (setf slot-names :all))
  (etypecase slot-names
    (null nil)
    (keyword
     ;; slot-names is the retrieval type of the join-slot or :all
     (get-join-slots class slot-names))
    ((or symbol list)
     (loop for slot in (listify slot-names)
           for def = (find-slot-by-name class slot)
           when (and def (join-slot-p def))
           collecting def
           unless (and def (join-slot-p def))
           do (warn "Unable to find join slot named ~S in class ~S." slot class)))))

(defun get-joined-objects (objects slotdef &key force-p
                                           (batch-size *default-update-objects-max-len*))
  "Given a list of objects and a join slot-def get the objects that need to be
   joined to the input objects

   we will query in batches as large as batch-size"
  (when (join-slot-p slotdef)
    (let* ((slot-name (to-slot-name slotdef))
           (join-class (join-slot-class-name slotdef))
           (home-key (join-slot-info-value slotdef :home-key))
           (foreign-key (join-slot-info-value slotdef :foreign-key))
           (foreign-key-values
             (remove-duplicates
              (loop for object in (listify objects)
                    for hk = (slot-value object home-key)
                    when (or force-p
                             (not (slot-boundp object slot-name)))
                    collect hk)
              :test #'equal)))
      ;; we want to retrieve at most batch-size objects per query
      (flet ((fetch (keys)
               (find-all
                (list join-class)
                :where (make-instance
                        'sql-relational-exp
                        :operator 'in
                        :sub-expressions (list (sql-expression :attribute foreign-key)
                                               keys))
                :result-types :auto
                :flatp t)))
        (if (null batch-size)
            (fetch foreign-key-values)
            (loop
              for keys = (pop-n foreign-key-values batch-size)
              while keys
              nconcing (fetch keys)))))))

(defun %object-joins-from-list (object slot joins force-p )
  "Given a list of objects that we are trying to join to, pull the correct
   ones for this object"
  (when (or force-p (not (slot-boundp object (to-slot-name slot))))
    (let ((home-key (join-slot-info-value slot :home-key))
          (foreign-key (join-slot-info-value slot :foreign-key)))
      (loop for join in joins
            when (equal (slot-value join foreign-key)
                        (slot-value object home-key))
            collect join))))

(defun update-objects-joins (objects &key (slots :immediate) (force-p t)
                                     class-name (max-len *default-update-objects-max-len*))
  "Updates from the records of the appropriate database tables the join slots
   specified by SLOTS in the supplied list of View Class instances OBJECTS.

   A simpler method of causing a join-slot to be requeried is to set it to
   unbound, then request it again.  This function has efficiency gains where
   join-objects are shared among the `objects` (querying all join-objects,
   then attaching them appropriately to each of the `objects`)

   SLOTS can be one of:

    * :immediate (DEFAULT) - refresh join slots created with :retrieval :immediate
    * :deferred - refresh join slots created with :retrieval :deferred
    * :all,t - refresh all join slots regardless of :retrieval
    * list of symbols - which explicit slots to refresh
    * a single symobl - what slot to refresh

   CLASS-NAME is used to specify the View Class of all instance in OBJECTS and
   default to nil which means that the class of the first instance in OBJECTS
   is used.

   FORCE-P is t by default which means that all join slots are updated whereas
   a value of nil means that only unbound join slots are updated.

   MAX-LEN defaults to *DEFAULT-UPDATE-OBJECTS-MAX-LEN* When non-nil this is
   essentially a batch size for the max number of objects to query from the
   database at a time.  If we need more than max-len we loop till we have all
   the objects"
  (assert (or (null max-len) (plusp max-len)))
  (when objects
    (defaulting class-name (class-name (class-of (first objects))))
    (let* ((class (find-class class-name))
           (slotdefs (%update-objects-joins-slot-defs class slots)))
      (loop for slotdef in slotdefs
            ;; all the joins we will need for *all* the objects
            ;; which then get filtered below for each object
            for joins = (unless (join-slot-info-value slotdef :target-slot)
                          (get-joined-objects objects slotdef
                                              :force-p force-p :batch-size max-len))
            do (loop for object in objects
                     for these-joins = ;; the joins just for this object (filtered from above)
                                       ;; or retrieved via fault-join-target-slot
                        (or (%object-joins-from-list object slotdef joins force-p)
                            (when (join-slot-info-value slotdef :target-slot)
                              (fault-join-target-slot class object slotdef)))
                     ;; when this object has joined-objects copy them in to the correct slot
                     do (when these-joins
                          (setf (easy-slot-value object slotdef)
                                (if (join-slot-info-value slotdef :set)
                                    these-joins
                                    (first these-joins))))))))
  (values))

(defun fault-join-slot-raw (class object slot-def)
  (let* ((dbi (view-class-slot-db-info slot-def))
         (jc (gethash :join-class dbi)))
    (let ((jq (join-qualifier class object slot-def)))
      (when jq
        (select jc :where jq :flatp t :result-types nil
                :database (choose-database-for-instance object))))))

(defun fault-join-slot (class object slot-def)
  (let* ((dbi (view-class-slot-db-info slot-def))
         (ts (gethash :target-slot dbi))
         (dbi-set (gethash :set dbi)))
    (if (and ts dbi-set)
        (fault-join-target-slot class object slot-def)
        (let ((res (fault-join-slot-raw class object slot-def)))
          (when res
            (cond
              ((and ts (not dbi-set))
               (mapcar (lambda (obj) (slot-value obj ts)) res))
              ((and (not ts) (not dbi-set))
               (car res))
              ((and (not ts) dbi-set)
               res)))))))

(defun update-fault-join-normalized-slot (class object slot-def)
  (if (and (normalizedp class) (key-slot-p slot-def))
      (setf (easy-slot-value object slot-def)
            (normalized-key-value object))
      (update-slot-from-record object slot-def)))

(defun all-home-keys-have-values-p (object slot-def)
  "Do all of the home-keys have values ?"
  (let ((home-keys (join-slot-info-value slot-def :home-key)))
    (loop for key in (listify home-keys)
          always (easy-slot-value object key))))

(defun join-qualifier (class object slot-def)
  "Builds the join where clause based on the keys of the join slot and values
   of the object"
  (declare (ignore class))
  (let* ((jc (join-slot-class slot-def))
         ;;(ts (gethash :target-slot dbi))
         ;;(tsdef (if ts (slotdef-for-slot-with-class ts jc)))
         (foreign-keys (listify (join-slot-info-value slot-def :foreign-key)))
         (home-keys (listify (join-slot-info-value slot-def :home-key))))
    (when (all-home-keys-have-values-p object slot-def)
      (clsql-ands
       (loop for hk in home-keys
             for fk in foreign-keys
             for fksd = (slotdef-for-slot-with-class fk jc)
             for fk-sql = (typecase fk
                            (symbol
                             (sql-expression
                              :attribute (database-identifier fksd nil)
                              :table (database-identifier jc nil)))
                            (t fk))
             for hk-val = (typecase hk
                            ((or symbol
                                 view-class-effective-slot-definition
                                 view-class-direct-slot-definition)
                             (easy-slot-value object hk))
                            (t hk))
             collect (sql-operation '== fk-sql hk-val))))))

(defmethod select-table-sql-expr ((table T))
  "Turns an object representing a table into the :from part of the sql expression that will be executed "
  (sql-expression :table (view-table table)))

(defun select-reference-equal (r1 r2)
  "determines if two sql select references are equal
   using database identifier equal"
  (flet ((id-of (r)
           (etypecase r
             (cons (cdr r))
             (sql-ident-attribute r))))
    (database-identifier-equal (id-of r1) (id-of r2))))

(defun join-slot-qualifier (class join-slot)
  "Creates a sql-expression expressing the join between the home-key on the table
   and its respective key on the joined-to-table"
  (sql-operation
   '==
   (sql-expression
    :attribute (join-slot-info-value join-slot :foreign-key)
    :table (view-table (join-slot-class join-slot)))
   (sql-expression
    :attribute (join-slot-info-value join-slot :home-key)
    :table (view-table class))))

(defun all-immediate-join-classes-for (classes)
  "returns a list of all join-classes needed for a list of classes"
  (loop for class in (listify classes)
        appending (loop for slot in (immediate-join-slots class)
                        collect (join-slot-class slot))))

(defun %tables-for-query (classes from where inner-joins)
  "Given lists of classes froms wheres and inner-join compile a list
   of tables that should appear in the FROM section of the query.

   This includes any immediate join classes from each of the classes"
  (let ((inner-join-tables (collect-table-refs (listify inner-joins))))
    (loop for tbl in (append
                      (mapcar #'select-table-sql-expr classes)
                      (mapcar #'select-table-sql-expr
                              (all-immediate-join-classes-for classes))
                      (collect-table-refs (listify where))
                      (collect-table-refs (listify from)))
          when (and tbl
                    (not (find tbl rtn :test #'database-identifier-equal))
                    ;; TODO: inner-join is currently hacky as can be
                    (not (find tbl inner-join-tables :test #'database-identifier-equal)))
          collect tbl into rtn
          finally (return rtn))))


(defclass select-list ()
  ((view-class :accessor view-class :initarg :view-class :initform nil)
   (select-list :accessor select-list :initarg :select-list :initform nil)
   (slot-list :accessor slot-list :initarg :slot-list :initform nil)
   (joins :accessor joins :initarg :joins :initform nil)
   (join-slots :accessor join-slots :initarg :join-slots :initform nil))
  (:documentation
   "Collects the classes, slots and their respective sql representations
    so that update-instance-from-recors, find-all, build-objects can share this
    info and calculate it once.  Joins are select-lists for each immediate join-slot
    but only if make-select-list is called with do-joins-p"))

(defmethod view-table ((o select-list))
  (view-table (view-class o)))

(defmethod sql-table ((o select-list))
  (sql-expression :table (view-table o)))

(defmethod filter-select-list ((c clsql-sys::standard-db-object)
                               (sl clsql-sys::select-list)
                               database)
  sl)

(defun make-select-list (class-and-slots &key (do-joins-p nil)
                                         (database *default-database*))
  "Make a select-list for the current class (or class-and-slots) object."
  (let* ((class-and-slots
           (etypecase class-and-slots
             (class-and-slots class-and-slots)
             ((or symbol standard-db-class)
              ;; find the first class with slots for us to select (this should be)
              ;; the first of its classes / parent-classes with slots
              (first (reverse (view-classes-and-storable-slots
                               (to-class class-and-slots)
                                :to-database-p nil))))))
         (class (view-class class-and-slots))
         (join-slots (when do-joins-p (immediate-join-slots class))))
    (multiple-value-bind (slots sqls)
        (loop for slot in (slot-defs class-and-slots)
              for sql = (generate-attribute-reference class slot)
              collect slot into slots
              collect sql into sqls
              finally (return (values slots sqls)))
      (unless slots
        (error "No slots of type :base in view-class ~A" (class-name class)))
      (let ((sl (make-instance
                 'select-list
                 :view-class class
                 :select-list sqls
                 :slot-list slots
                 :join-slots join-slots
                 ;; only do a single layer of join objects
                 :joins (when do-joins-p
                          (loop for js in join-slots
                                collect (make-select-list
                                         (join-slot-class js)
                                         :do-joins-p nil
                                         :database database))))))
        (filter-select-list (make-instance class) sl database)
        sl))))

(defun full-select-list ( select-lists )
  "Returns a list of sql-ref of things to select for the given classes

   THIS NEEDS TO MATCH THE ORDER OF build-objects
  "
  (loop for s in (listify select-lists)
        appending (select-list s)
        appending (loop for join in (joins s)
                        appending (select-list join))))

(defun build-objects (select-lists row database &optional existing-instances)
  "Used by find-all to build objects.

   THIS NEEDS TO MATCH THE ORDER OF FULL-SELECT-LIST

   TODO: this caching scheme seems bad for a number of reasons
    * order is not guaranteed so references being held by one object
      might change to represent a different database row (seems HIGHLY
      suspect)
    * also join objects are overwritten rather than refreshed

   TODO: the way we handle immediate joins seems only valid if it is a single
      object.  I suspect that making a :set :immediate join column would result
      in an invalid number of objects returned from the database, because there
      would be multiple rows per object, but we would return an object per row
   "
  (setf existing-instances (listify existing-instances))
  (loop
    for select-list in select-lists
    for class = (view-class select-list)
    for existing = (pop existing-instances)
    for object = (or existing
                     (make-instance class :view-database database))
    do (loop for slot in (slot-list select-list)
             do (update-slot-from-db-value object slot (pop row)))
    do (loop for join-slot in (join-slots select-list)
             for join in (joins select-list)
             for join-class = (view-class join)
             for join-object =
                (setf (easy-slot-value object join-slot)
                      (make-instance join-class))
             do (loop for slot in (slot-list join)
                      do (update-slot-from-db-value join-object slot (pop row))))
    do (when existing (instance-refreshed object))
        collect object))

(defun find-all (view-classes
                 &rest args
                 &key all set-operation distinct from where group-by having
                 order-by offset limit refresh flatp result-types
                 inner-join on
                 (database *default-database*)
                 instances parameters)
  "Called by SELECT to generate object query results when the
  View Classes VIEW-CLASSES are passed as arguments to SELECT.

   TODO: the caching scheme of passing in instances and overwriting their
         values seems bad for a number of reasons
    * order is not guaranteed so references being held by one object
      might change to represent a different database row (seems HIGHLY
      suspect)

   TODO: the way we handle immediate joins seems only valid if it is a single
      object.  I suspect that making a :set :immediate join column would result
      in an invalid number of objects returned from the database, because there
      would be multiple objects returned from the database
  "
  (declare (ignore all set-operation group-by having offset limit on parameters
                   distinct order-by)
           (dynamic-extent args))
  (let* ((args (filter-plist
                args :from :where :flatp :additional-fields :result-types :instances))
         (*db-deserializing* t)
         (sclasses (mapcar #'to-class view-classes))
         (tables (%tables-for-query sclasses from where inner-join))
         (join-where
           (loop for class in sclasses
                 appending (loop for slot in (immediate-join-slots class)
                                 collect (join-slot-qualifier class slot))))
         (select-lists (loop for class in sclasses
                             collect (make-select-list class :do-joins-p t :database database)))
         (full-select-list (full-select-list select-lists))
         (where (clsql-ands (append (listify where) (listify join-where))))
         #|
          (_ (format t "~&sclasses: ~W~%ijc: ~W~%tables: ~W~%"
                    sclasses immediate-join-classes tables))
         |#
         (rows (apply #'select
                      (append full-select-list
                              (list :from tables
                                    :result-types result-types
                                    :where where)
                              args)))
         (return-objects
           (loop for row in rows
                 for old-objs = (pop instances)
                 for objs = (build-objects select-lists row database
                                           (when refresh old-objs))
                 collecting (if flatp
                                (delist-if-single objs)
                                objs))))
    return-objects))

(defmethod instance-refreshed ((instance standard-db-object)))

(defvar *default-caching* t
  "Controls whether SELECT caches objects by default. The CommonSQL
specification states caching is on by default.")

(defun select (&rest select-all-args)
  "Executes a query on DATABASE, which has a default value of
*DEFAULT-DATABASE*, specified by the SQL expressions supplied
using the remaining arguments in SELECT-ALL-ARGS. The SELECT
argument can be used to generate queries in both functional and
object oriented contexts.

In the functional case, the required arguments specify the
columns selected by the query and may be symbolic SQL expressions
or strings representing attribute identifiers. Type modified
identifiers indicate that the values selected from the specified
column are converted to the specified lisp type. The keyword
arguments ALL, DISTINCT, FROM, GROUP-by, HAVING, ORDER-BY,
SET-OPERATION and WHERE are used to specify, using the symbolic
SQL syntax, the corresponding components of the SQL query
generated by the call to SELECT. RESULT-TYPES is a list of
symbols which specifies the lisp type for each field returned by
the query. If RESULT-TYPES is nil all results are returned as
strings whereas the default value of :auto means that the lisp
types are automatically computed for each field. FIELD-NAMES is t
by default which means that the second value returned is a list
of strings representing the columns selected by the query. If
FIELD-NAMES is nil, the list of column names is not returned as a
second value.

In the object oriented case, the required arguments to SELECT are
symbols denoting View Classes which specify the database tables
to query. In this case, SELECT returns a list of View Class
instances whose slots are set from the attribute values of the
records in the specified table. Slot-value is a legal operator
which can be employed as part of the symbolic SQL syntax used in
the WHERE keyword argument to SELECT. REFRESH is nil by default
which means that the View Class instances returned are retrieved
from a cache if an equivalent call to SELECT has previously been
issued. If REFRESH is true, the View Class instances returned are
updated as necessary from the database and the generic function
INSTANCE-REFRESHED is called to perform any necessary operations
on the updated instances.

In both object oriented and functional contexts, FLATP has a
default value of nil which means that the results are returned as
a list of lists. If FLATP is t and only one result is returned
for each record selected in the query, the results are returned
as elements of a list."
  (multiple-value-bind (target-args qualifier-args)
      (query-get-selections select-all-args)
    (unless (or *default-database* (getf qualifier-args :database))
      (signal-no-database-error nil))

    (let ((caching (getf qualifier-args :caching *default-caching*))
          (result-types (getf qualifier-args :result-types :auto))
          (refresh (getf qualifier-args :refresh nil))
          (database (getf qualifier-args :database *default-database*)))

      (cond
        ((and target-args
              (every #'(lambda (arg)
                         (and (symbolp arg)
                              (find-class arg nil)))
                     target-args))

         (setf qualifier-args (filter-plist qualifier-args :caching :refresh :result-types))

         ;; Add explicity table name to order-by if not specified and only
         ;; one selected table. This is required so FIND-ALL won't duplicate
         ;; the field
         (let ((order-by (getf qualifier-args :order-by)))
           (when (and order-by (= 1 (length target-args)))
             (let ((table-name (view-table (find-class (car target-args))))
                   (order-by-list (copy-seq (listify order-by))))
               (labels ((sv (val name) (ignore-errors (slot-value val name)))
                        (set-table-if-needed (val)
                          (typecase val
                            (sql-ident-attribute
                             (handler-case
                                 (if (sv val 'qualifier)
                                     val
                                     (make-instance 'sql-ident-attribute
                                                    :name (sv val 'name)
                                                    :qualifier table-name))
                               (simple-error ()
                                 ;; TODO: Check for a specific error we expect
                                 )))
                            (cons (cons (set-table-if-needed (car val))
                                        (cdr val)))
                            (t val))))
                 (setf order-by-list
                       (loop for i from 0 below (length order-by-list)
                             for id in order-by-list
                             collect (set-table-if-needed id))))
               (setf (getf qualifier-args :order-by) order-by-list))))

         (cond
           ((null caching)
            (apply #'find-all target-args :result-types result-types :refresh refresh qualifier-args))
           (t
            (let ((cached (records-cache-results target-args qualifier-args database)))
              (if (and cached (not refresh))
                  cached
                  (let ((results (apply #'find-all target-args
                                        :result-types :auto :refresh refresh
                                        :instances cached
                                        qualifier-args)))
                    (setf (records-cache-results target-args qualifier-args database) results)

                    results))))))
        (t
         (let* ((expr (apply #'make-query select-all-args))
                (parameters (second (member :parameters select-all-args)))
                (specified-types
                  (mapcar #'(lambda (attrib)
                              (if (typep attrib 'sql-ident-attribute)
                                  (let ((type (slot-value attrib 'type)))
                                    (if type
                                        type
                                        t))
                                  t))
                          (slot-value expr 'selections)))
                (flatp (getf qualifier-args :flatp))
                (field-names (getf qualifier-args :field-names t)))

           (when parameters
             (setf expr (command-object (sql-output expr database) parameters)))
           (query expr :flatp flatp
                       :result-types
                       ;; specifying a type for an attribute overrides result-types
                       (if (some #'(lambda (x) (not (eq t x))) specified-types)
                           specified-types
                           result-types)
                       :field-names field-names
                       :database database)))))))

(defun compute-records-cache-key (targets qualifiers)
  (list targets
        (do ((args *select-arguments* (cdr args))
             (results nil))
            ((null args) results)
          (let* ((arg (car args))
                 (value (getf qualifiers arg)))
            (when value
              (push (list arg
                          (typecase value
                            (cons (cons (sql (car value)) (cdr value)))
                            (%sql-expression (sql value))
                            (t value)))
                    results))))))

(defun records-cache-results (targets qualifiers database)
  (when (record-caches database)
    (gethash (compute-records-cache-key targets qualifiers) (record-caches database))))

(defun (setf records-cache-results) (results targets qualifiers database)
  (unless (record-caches database)
    (setf (record-caches database)
          (make-weak-hash-table :test 'equal)))
  (setf (gethash (compute-records-cache-key (copy-list targets) qualifiers)
                 (record-caches database)) results)
  results)



;;; Serialization functions

(defun write-instance-to-stream (obj stream)
  "Writes an instance to a stream where it can be later be read.
NOTE: an error will occur if a slot holds a value which can not be written readably."
  (let* ((class (class-of obj))
         (alist '()))
    (dolist (slot (ordered-class-slots (class-of obj)))
      (let ((name (slot-definition-name slot)))
        (when (and (not (eq 'view-database name))
                   (slot-boundp obj name))
          (push (cons name (slot-value obj name)) alist))))
    (setq alist (reverse alist))
    (write (cons (class-name class) alist) :stream stream :readably t))
  obj)

(defun read-instance-from-stream (stream)
  (let ((raw (read stream nil nil)))
    (when raw
      (let ((obj (make-instance (car raw))))
        (dolist (pair (cdr raw))
          (setf (slot-value obj (car pair)) (cdr pair)))
        obj))))
