;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; Generic ODBC layer, used by db-odbc and db-aodbc backends
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defclass generic-odbc-database (database)
  ((dbi-package :initarg :dbi-package :reader dbi-package)
   (odbc-conn :initarg :odbc-conn :initform nil :accessor odbc-conn)
   (disconnect-fn :reader disconnect-fn)
   (sql-fn :reader sql-fn)
   (close-query-fn :reader close-query-fn)
   (fetch-row :reader fetch-row-fn)
   (list-all-database-tables-fn :reader list-all-database-tables-fn)
   (list-all-table-columns-fn :reader list-all-table-columns-fn)
   (odbc-db-type :accessor database-odbc-db-type :initarg :odbc-db-type ))
  (:documentation "Encapsulate same behavior across odbc and aodbc backends."))

(defmethod initialize-instance :after ((db generic-odbc-database)
                                        &rest all-keys)
  (declare (ignore all-keys))
  (unless (slot-boundp db 'dbi-package)
    (error "dbi-package not specified."))
  (let ((pkg (slot-value db 'dbi-package)))
    (unless pkg
      (error "dbi-package is nil."))
    (setf (slot-value db 'disconnect-fn)
          (intern (symbol-name '#:disconnect) pkg)
          (slot-value db 'sql-fn)
          (intern (symbol-name '#:sql) pkg)
          (slot-value db 'close-query-fn)
          (intern (symbol-name '#:close-query) pkg)
          (slot-value db 'fetch-row)
          (intern (symbol-name '#:fetch-row) pkg)
          (slot-value db 'list-all-database-tables-fn)
          (intern (symbol-name '#:list-all-database-tables) pkg)
          (slot-value db 'list-all-table-columns-fn)
          (intern (symbol-name '#:list-all-table-columns) pkg))))

;;; Type methods

(defmethod database-get-type-specifier ((type symbol) args database
                                        (db-type (eql :mssql)))
  "Special database types for MSSQL backends"
  (declare (ignore database db-type args))
  (case type
    (wall-time "DATETIME")
    (date "SMALLDATETIME")
    ((generalized-boolean boolean) "BIT")
    ((longchar text) "ntext")
    ((varchar string)
     (if args
         (format nil "NVARCHAR(~A)" (car args))
         (format nil "NVARCHAR(~D)" *default-string-length*)))
    (t (call-next-method))))

;;; Generation of SQL strings from lisp expressions

(defmethod database-output-sql ((tee (eql t)) (database generic-odbc-database))
  (case (database-underlying-type database)
    (:mssql "1")
    (t "'Y'")))

;;; Database backend capabilities

(defmethod db-type-use-fully-qualified-column-on-drop-index? ((db-type (eql :mssql)))
  t)

(defmethod db-type-has-boolean-where? ((db-type (eql :mssql)))
  nil)

(defmethod db-type-has-intersect? ((db-type (eql :mssql)))
  nil)

(defmethod db-type-has-except? ((db-type (eql :mssql)))
  nil)

;;; Backend methods

(defmethod database-disconnect ((database generic-odbc-database))
  (funcall (disconnect-fn database) (odbc-conn database))
  (setf (odbc-conn database) nil)
  t)

(defmethod database-query (query-expression (database generic-odbc-database)
                           result-types field-names)
  (handler-case
      (funcall (sql-fn database)
               query-expression :db (odbc-conn database)
               :result-types result-types
               :column-names field-names)
    #+ignore
    (error ()
      (error 'sql-database-data-error
             :database database
             :expression query-expression
             :message "Query failed"))))


(defmethod database-execute-command (sql-expression (database generic-odbc-database))
  (handler-case
      (funcall (sql-fn database)
               sql-expression :db (odbc-conn database))
    #+ignore
    (sql-error (e)
      (error e))
    #+ignore
    (error ()
      (error 'sql-database-data-error
             :database database
             :expression sql-expression
             :message "Execute command failed"))))


(defstruct odbc-result-set
  (query nil)
  (types nil)
  (full-set nil :type boolean))




(defmethod database-query-result-set ((query-expression string)
                                      (database generic-odbc-database)
                                      &key full-set result-types)
  (handler-case
      (multiple-value-bind (query column-names)
          (funcall (sql-fn database)
                   query-expression
                   :db (odbc-conn database)
                   :row-count nil
                   :column-names t
                   :query t
                   :result-types result-types)
        (values
         (make-odbc-result-set :query query :full-set full-set
                               :types result-types)
         (length column-names)
         nil ;; not able to return number of rows with odbc
         ))
    (error ()
      (error 'sql-database-data-error
             :database database
             :expression query-expression
             :message "Query result set failed"))))

(defmethod database-dump-result-set (result-set (database generic-odbc-database))
  (funcall (close-query-fn database) (odbc-result-set-query result-set))
  t)

(defmethod database-store-next-row (result-set
                                    (database generic-odbc-database)
                                    list)
  (let ((row (funcall (fetch-row-fn database)
                      (odbc-result-set-query result-set) nil 'eof)))
    (if (eq row 'eof)
        nil
      (progn
        (loop for elem in row
            for rest on list
            do
              (setf (car rest) elem))
        list))))


(defun %database-list-* (database type owner)
  "Internal function used by database-list-tables and
database-list-views"
  (multiple-value-bind (rows col-names)
      (funcall (list-all-database-tables-fn database) :db (odbc-conn database))
    (declare (ignore col-names))
    ;; http://msdn.microsoft.com/en-us/library/ms711831%28VS.85%29.aspx
    ;; TABLE_SCHEM is hard-coded in second column by ODBC Driver Manager
    ;; TABLE_NAME in third column, TABLE_TYPE in fourth column
    (loop for (category schema name ttype . rest) in rows
	  when (and (string-equal type ttype)
		    (or (null owner) (string-equal owner schema))
		    ;; unless requesting by name, skip system schema
		    (not (and (null owner)
			      (member schema '("information_schema" "sys")
				      :test #'string-equal)))
		    ;; skip system specific tables in mssql2000
		    (not (and (eql :mssql (database-underlying-type database))
			      (member name '("dtproperties" "sysconstraints"
					     "syssegments")
				      :test #'string-equal))))
	    collect name)))

(defmethod database-list-tables ((database generic-odbc-database)
				 &key (owner nil))
  "Since ODBC doesn't expose the owner we use that parameter to filter
on schema since that's what tends to be exposed. Some DBs like mssql
2000 conflate the two so at least there it works nicely."
  (%database-list-* database "TABLE" owner))


(defmethod database-list-views ((database generic-odbc-database)
				&key (owner nil))
  "Since ODBC doesn't expose the owner we use that parameter to filter
on schema since that's what tends to be exposed. Some DBs like mssql
2000 conflate the two so at least there it works nicely."
  (%database-list-* database "VIEW" owner))


(defmethod database-list-attributes ((table %database-identifier) (database generic-odbc-database)
                                     &key (owner nil)
                                     &aux (table (unescaped-database-identifier table)))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (funcall (list-all-table-columns-fn database) table
               :db (odbc-conn database))
    (declare (ignore col-names))
    ;; COLUMN_NAME is hard-coded by odbc spec as fourth position
    (loop for row in rows
        collect (fourth row))))

(defmethod database-attribute-type ((attribute %database-identifier) (table %database-identifier)
                                    (database generic-odbc-database)
                                    &key (owner nil)
                                    &aux (table (unescaped-database-identifier table))
                                    (attribute (unescaped-database-identifier attribute)))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (funcall (list-all-table-columns-fn database) table
               :db (odbc-conn database))
    (declare (ignore col-names))
    ;; COLUMN_NAME is hard-coded by odbc spec as fourth position
    ;; TYPE_NAME is the sixth column
    ;; PRECISION/COLUMN_SIZE is the seventh column
    ;; SCALE/DECIMAL_DIGITS is the ninth column
    ;; NULLABLE is the eleventh column
    (loop for row in rows
        when (string-equal attribute (fourth row))
        do
        (let ((size (seventh row))
              (precision (ninth row))
              (scale (nth 10 row)))
          (return (values (ensure-keyword (sixth row))
                          (when size (parse-integer size))
                          (when precision (parse-integer precision))
                          (when scale (parse-integer scale))))))))

(defmethod database-last-auto-increment-id
    ((database generic-odbc-database) table column)
  (case (database-underlying-type database)
    (:mssql
     (first (clsql:query "SELECT SCOPE_IDENTITY()"
                         :flatp t
                         :database database
                         :result-types '(:int))))
    (t (if (next-method-p)
           (call-next-method)))))

(defmethod clsql-sys:db-type-has-auto-increment? ((db-underlying-type (eql :mssql)))
  t)
