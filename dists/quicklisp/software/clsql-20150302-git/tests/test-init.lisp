;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-init.lisp
;;;; Authors: Marcus Pearce <m.t.pearce@city.ac.uk>, Kevin Rosenberg
;;;; Created: 30/03/2004
;;;;
;;;; Initialisation utilities for running regression tests on CLSQL.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; ======================================================================

(in-package #:clsql-tests)

(defvar *report-stream* *standard-output* "Stream to send text report.")
(defvar *sexp-report-stream* nil "Stream to send sexp report.")
(defvar *rt-internal*)
(defvar *rt-basic*)
(defvar *rt-connection*)
(defvar *rt-fddl*)
(defvar *rt-fdml*)
(defvar *rt-ooddl*)
(defvar *rt-oodml*)
(defvar *rt-syntax*)
(defvar *rt-time*)
(defvar *rt-pool*)
;; Below must be set as nil since test-i18n.lisp is not loaded on all platforms.
(defvar *rt-i18n* nil)

(defvar *test-database-type* nil)
(defvar *test-database-underlying-type* nil)
(defvar *test-database-user* nil)
(defvar *test-false-database-user* "adsfjalsdkfjlakjsdfl"
  "For testing ownership, a user that isn't the owner.")
(defvar *test-start-utime* nil)
(defvar *test-connection-spec* nil)
(defvar *test-connection-db-type* nil)
(defvar *test-report-width* 80 "Width of test report in ems.")


(defun find-test-connection-spec (db-type &key position)
  (nth (or position 0)
       (db-type-spec db-type (read-specs))))

(defun test-connect
    (&key
     (db-type *test-database-type* db-type-p)
     position pool spec)
  (unless spec
    (setf spec
          (or (and (null db-type-p) *test-connection-spec*)
              (find-test-connection-spec db-type :position position))))
  (when *default-database*
    (disconnect :database *default-database*))
  (setf *test-database-type* db-type
        *test-database-user*
        (cond
          ((member db-type '(:oracle :odbc :aodbc)) (second spec))
          ((>= (length spec) 3) (third spec)))
        *test-connection-spec* spec
        *default-database*
        (clsql:connect
         spec
         :database-type db-type
         :make-default t
         :if-exists :old
         :pool pool)
        *test-database-underlying-type*
        (clsql-sys:database-underlying-type *default-database*))
  *default-database*)

(defun test-setup-database (db-type &key (spec (find-test-connection-spec db-type)))
  (when (clsql-sys:db-backend-has-create/destroy-db? db-type)
    (ignore-errors (destroy-database spec :database-type db-type))
    (ignore-errors (create-database spec :database-type db-type)))

  ;; Connect to the database
  (test-connect :db-type db-type :spec spec)
  ;; Ensure database is empty
  (truncate-database :database *default-database*)

  ;; If Postgres, turn off notices to console
  (when (eql db-type :postgresql)
    (clsql:execute-command "SET client_min_messages = WARNING"))

  *default-database*)

(defun default-suites ()
  "The default list of tests to run."
  (append *rt-connection* *rt-basic* *rt-fddl* *rt-fdml*
	  *rt-ooddl* *rt-oodml* *rt-syntax* *rt-time* *rt-i18n*))

(defun internal-suites ()
  "The default internal suites that should run without any specific backend"
  (append *rt-internal* *rt-pool*))


(defvar *error-count* 0)
(defvar *error-list* nil)

(defun run-function-append-report-file (function report-file)
    (let* ((report-path (etypecase report-file
                        (pathname report-file)
                        (string (parse-namestring report-file))))
         (sexp-report-path (make-pathname :defaults report-path
                                          :type "sexp")))
      (with-open-file (rs report-path :direction :output
                          :if-exists :append
                      :if-does-not-exist :create)
        (with-open-file (srs sexp-report-path :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
          (funcall function :report-stream rs :sexp-report-stream srs)))))

(defun run-tests-append-report-file (report-file)
  (run-function-append-report-file 'run-tests report-file))


(defun run-tests (&key (report-stream *standard-output*) (sexp-report-stream nil)
		  (suites (append (internal-suites) (default-suites))))
  ;; clear SQL-OUTPUT cache
  (setq clsql-sys::*output-hash* (make-hash-table :test #'equal))
  (setf *test-database-underlying-type* nil)
  (let ((specs (read-specs))
        (*report-stream* report-stream)
        (*sexp-report-stream* sexp-report-stream)
        (*error-count* 0)
        (*error-list* nil))
    (unless specs
      (warn "Not running tests because test configuration file is missing")
      (return-from run-tests :skipped))
    (load-necessary-systems specs)
    ;;run the internal suites
    (do-tests-for-internals :suites (intersection suites (internal-suites)))
    ;; run backend-specific tests
    (let ((suites (intersection suites (default-suites))))
      (when suites
        (dolist (db-type +all-db-types+)
          (dolist (spec (db-type-spec db-type specs))            
            (format report-stream "~%~%Start Running Tests Against: ~A ~A~%~%" db-type (ignore-errors (subseq spec 0 2)))
            (do-tests-for-backend db-type spec :suites suites)
            (format report-stream  "~%~%Finished Running Tests Against: ~A ~A~%~%" db-type (ignore-errors (subseq spec 0 2)))))))
    (zerop *error-count*)))

(defun load-necessary-systems (specs)
  (dolist (db-type +all-db-types+)
    (when (db-type-spec db-type specs)
      (clsql-sys:initialize-database-type :database-type db-type))))

(defun write-report-banner (report-type db-type stream db-name)
  (format stream
          "~&
******************************************************************************
***     CLSQL ~A begun at ~A
***     ~A
***     ~A on ~A
***     Database ~:@(~A~)
***     Type: ~:@(~A~) backend~A.
******************************************************************************
"
          report-type
          (clsql:format-time
           nil
           (clsql:utime->time (get-universal-time)))
          (lisp-implementation-type)
          (lisp-implementation-version)
          (machine-type)
	  db-name
          db-type
          (if (not (eq db-type *test-database-underlying-type*))
              (format nil " with underlying type ~:@(~A~)"
                      *test-database-underlying-type*)
              "")
          ))

(defun do-tests-for-internals (&key (suites (internal-suites)))
  (write-report-banner "Test Suite" "CLSQL Internals" *report-stream*
                       "N/A")
  (%do-tests suites nil))

(defun %do-tests (test-forms db-type)
  (regression-test:rem-all-tests)
  (dolist (test-form test-forms)
    (eval test-form))

  (let* ((cl:*print-right-margin* *test-report-width*)
         (remaining (regression-test:do-tests *report-stream*)))
    (when (regression-test:pending-tests)
      (incf *error-count* (length remaining))))

  (let ((sexp-error (list db-type
                          *test-database-underlying-type*
                          (get-universal-time)
                          (length test-forms)
                          (regression-test:pending-tests)
                          (lisp-implementation-type)
                          (lisp-implementation-version)
                          (machine-type))))
    (when *sexp-report-stream*
      (write sexp-error :stream *sexp-report-stream* :readably t))
    (push sexp-error *error-list*))
  )

(defun do-tests-for-backend (db-type spec &key
			     (suites (default-suites)) )
  (test-setup-database db-type :spec spec)
  (unwind-protect
       (multiple-value-bind (test-forms skip-tests)
           (compute-tests-for-backend db-type *test-database-underlying-type* :suites suites)

           (write-report-banner "Test Suite" db-type *report-stream*
				(database-name-from-spec spec db-type))

         (%do-tests test-forms db-type)

           (format *report-stream* "~&Tests skipped:")
           (if skip-tests
               (let ((max-test-name (length (symbol-name (caar skip-tests)))))
                 (dolist (skipped (cdr skip-tests))
                   (let ((len (length (symbol-name (car skipped)))))
                     (when (> len max-test-name)
                       (setq max-test-name len))))
                 (let ((fmt (format nil "~~&  ~~~DA ~~A~~%" max-test-name)))
                   (dolist (skipped skip-tests)
                     ;; word-wrap the reason string field
                     (let* ((test (car skipped))
                            (reason (cdr skipped))
                            ;; (rlen (length reason))
                            (rwidth (max 20 (- (or *test-report-width* 80) max-test-name 3)))
                            (rwords (clsql-sys::delimited-string-to-list reason #\space t))
                            (rformat (format nil "~~{~~<~%~~1,~D:;~~A~~> ~~}" rwidth))
                            (rwrapped (format nil rformat rwords))
                            (rlines (clsql-sys::delimited-string-to-list rwrapped #\Newline t)))
                       (dolist (rline rlines)
                         (format *report-stream* fmt (if test
                                                         (prog1
                                                             test
                                                           (setq test nil))
                                                         "")
                                 rline))))))
               (format *report-stream* " None~%")))
    (disconnect)))


(defun compute-tests-for-backend (db-type db-underlying-type
				  &key (suites (default-suites)))
  (let ((test-forms '())
        (skip-tests '()))
    (dolist (test-form (if (listp suites) suites (list suites)))
      (let ((test (second test-form)))
        (cond
	  ((and (not (eql db-underlying-type :mysql))
		(clsql-sys:in test :connection/query-command
                              :basic/reallybigintegers/1
                              :connection/pool/procedure-mysql))
	   (push (cons test "known to work only in MySQL as yet.") skip-tests))
          ((and (null (clsql-sys:db-type-has-views? db-underlying-type))
                (clsql-sys:in test :fddl/view/1 :fddl/view/2 :fddl/view/3 :fddl/view/4))
           (push (cons test "views not supported.") skip-tests))
          ((and (null (clsql-sys:db-type-has-boolean-where? db-underlying-type))
                (clsql-sys:in test :fdml/select/11 :oodml/select/5))
           (push (cons test "boolean where not supported.") skip-tests))
          ((and (null (clsql-sys:db-type-has-subqueries? db-underlying-type))
                (clsql-sys:in test :fdml/select/5 :fdml/select/10
                              :fdml/select/32 :fdml/select/33))
           (push (cons test "subqueries not supported.") skip-tests))
          ((and (null (clsql-sys:db-type-transaction-capable? db-underlying-type
                                                    *default-database*))
                (clsql-sys:in test :fdml/transaction/1 :fdml/transaction/2 :fdml/transaction/3 :fdml/transaction/4))
           (push (cons test "transactions not supported.") skip-tests))
          ((and (null (clsql-sys:db-type-has-fancy-math? db-underlying-type))
                (clsql-sys:in test :fdml/select/1))
           (push (cons test "fancy math not supported.") skip-tests))
          ((and (eql *test-database-type* :sqlite)
                (clsql-sys:in test :fddl/view/4 :fdml/select/10
                                :fdml/select/21 :fdml/select/32
                                :fdml/select/33))
           (push (cons test "not supported by sqlite.") skip-tests))
          ((and (eql *test-database-type* :sqlite3)
                (clsql-sys:in test :fddl/view/4 :fdml/select/10
                              :fdml/select/21 :fdml/select/32
                              :fdml/select/33))
           (push (cons test "not supported by sqlite3.") skip-tests))
          ((and (not (clsql-sys:db-type-has-bigint? db-type))
                (clsql-sys:in test :basic/bigint/1))
           (push (cons test "bigint not supported.") skip-tests))
          ((and (eql *test-database-underlying-type* :mysql)
                (clsql-sys:in test :fdml/select/26))
           (push (cons test "string table aliases not supported on all MySQL versions.") skip-tests))
          ((and (eql *test-database-underlying-type* :mysql)
                (clsql-sys:in test :fdml/select/22 :fdml/query/5
                                :fdml/query/7 :fdml/query/8))
           (push (cons test "not supported by mysql.") skip-tests))
          ((and (null (clsql-sys:db-type-has-union? db-underlying-type))
                (clsql-sys:in test :fdml/query/6 :fdml/select/31))
           (push (cons test "union not supported") skip-tests))
          ((and (eq *test-database-type* :oracle)
                (clsql-sys:in test :fdml/query/8 :fdml/select/21
                              :fddl/table/6))
           (push (cons test "syntax not supported.") skip-tests))
          ((and (eq *test-database-type* :odbc)
                (eq *test-database-underlying-type* :postgresql)
                (clsql-sys:in test :fddl/owner/1 :fddl/owner/table
			      :fddl/owner/attributes
			      :fddl/owner/attribute-types
			      :fddl/owner/index
			      :fddl/owner/sequence))
          (push (cons test "table ownership not supported by postgresql odbc driver.") skip-tests))
          ((and (not (member *test-database-underlying-type*
                             '(:postgresql :oracle)))
                (clsql-sys:in test :fddl/owner/1 :fddl/owner/table
			      :fddl/owner/attributes
			      :fddl/owner/attribute-types
			      :fddl/owner/index
			      :fddl/owner/sequence))
	   (push (cons test "table ownership not supported.") skip-tests))
          ((and (null (clsql-sys:db-type-has-intersect? db-underlying-type))
                (clsql-sys:in test :fdml/query/7))
           (push (cons test "intersect not supported.") skip-tests))
          ((and (null (clsql-sys:db-type-has-except? db-underlying-type))
                (clsql-sys:in test :fdml/query/8))
           (push (cons test "except not supported.") skip-tests))
          ((and (eq *test-database-underlying-type* :mssql)
                (clsql-sys:in test :fdml/select/9))
           (push (cons test "mssql uses integer math for AVG.") skip-tests))
          ((and (not (member *test-database-underlying-type*
                             '(:postgresql :mysql :sqlite3 )))
                (clsql-sys:in test :fdml/select/37 :fdml/select/38))
           (push (cons test "LIMIT keyword not supported in SELECT.") skip-tests))
	  ((and (not (clsql-sys:db-type-has-auto-increment? db-underlying-type))
		(clsql-sys:in test :oodml/select/12 :oodml/select/13 :oodml/select/14
			      :oodml/select/15 :oodml/select/16 :oodml/select/17
			      :oodml/select/18 :oodml/select/19 :oodml/select/20
			      :oodml/select/21 :oodml/select/22 :oodml/select/23
			      :oodml/update-records/4 :oodml/update-records/4-slots
			      :oodml/update-records/5 :oodml/update-records/5-slots
			      :oodml/update-records/6 :oodml/update-records/7
			      :oodml/update-records/8 :oodml/update-records/9
			      :oodml/update-records/9-slots :oodml/update-records/10
			      :oodml/update-records/11 :OODML/UPDATE-RECORDS/12 :oodml/update-instance/3
			      :oodml/update-instance/4 :oodml/update-instance/5
			      :oodml/update-instance/6 :oodml/update-instance/7
			      :oodml/db-auto-sync/3 :oodml/db-auto-sync/4))
	   (push (cons test ":auto-increment not supported.") skip-tests))
         ((and (not (member *test-database-underlying-type*
			    '(:postgresql :postgresql-socket)))
               (clsql-sys:in test
                             :time/pg/fdml/usec :time/pg/oodml/no-usec :time/pg/oodml/usec))
          (push (cons test "Postgres specific test.")
                skip-tests))
         ((and (eql *test-database-type* :postgresql-socket3)
               (clsql-sys:in test :BASIC/SELECT/2 :basic/select/3))
          (push (cons test "Postgres-socket3 always auto types")
                skip-tests))
         ((and (eql *test-database-type* :postgresql-socket3)
               (clsql-sys:in test :fdml/select/18))
          (push (cons test "Postgres-socket3 doesnt support attribute based type coersion")
                skip-tests))
         ((and (eql *test-database-type* :postgresql-socket3)
               (clsql-sys:in test :basic/map/1 :basic/map/2 :basic/map/3 :basic/map/4
                :basic/do/1 :basic/do/2 :fdml/do-query/1 :fdml/map-query/1
                :fdml/map-query/2 :fdml/map-query/3 :fdml/map-query/4 :fdml/loop/1
                :fdml/loop/2 :fdml/loop/3
                ))
          (push (cons test "postgresql-socket3 doesnt support cursoring interface")
                skip-tests))
         ((and (member *test-database-underlying-type* '(:mysql))
               (clsql-sys:in test :time/cross-platform/msec
			     :time/cross-platform/usec/no-tz :time/cross-platform/usec/tz))
          (push (cons test "MySQL doesn't support fractional seconds on timestamp columns (http://forge.mysql.com/worklog/task.php?id=946).")
                skip-tests))
	  ((and (member *test-database-underlying-type* '(:mssql))
               (clsql-sys:in test :time/cross-platform/usec/no-tz :time/cross-platform/usec/tz))
          (push (cons test "MSSQL doesn't support micro-seconds on datetime columns.")
                skip-tests))
          (t
           (push test-form test-forms)))))
      (values (nreverse test-forms) (nreverse skip-tests))))

(defun rapid-load (type &optional (position 0))
  "Rapid load for interactive testing."
  (test-setup-database
   type
   :spec (find-test-connection-spec type :position position))
  *default-database*)

(defun rl ()
  (rapid-load :postgresql))

(defun rlm ()
  (rapid-load :mysql))

(defun rlo ()
  (rapid-load :oracle))
