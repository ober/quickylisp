;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          pool.lisp
;;;; Purpose:       Support function for connection pool
;;;; Programmers:   Kevin M. Rosenberg, Marc Battyani
;;;; Date Started:  Apr 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defparameter *db-pool-max-free-connections* 4
  "Threshold of free-connections in the pool before we disconnect a database
  rather than returning it to the pool.  NIL for no limit.  This is really a
  heuristic that should, on avg keep the free connections about this size.")

(defvar *db-pool* (make-hash-table :test #'equal))
(defvar *db-pool-lock* (make-process-lock "DB Pool lock"))

(defclass conn-pool ()
  ((connection-spec :accessor connection-spec :initarg :connection-spec)
   (database-type :accessor pool-database-type :initarg :pool-database-type)
   (free-connections :accessor free-connections :initform nil)
   (all-connections :accessor all-connections :initform nil)
   (lock :accessor conn-pool-lock
	 :initform (make-process-lock "Connection pool"))))




(defun acquire-from-pool (connection-spec database-type &optional pool (encoding *default-encoding*))
  "Try to find a working database connection in the pool or create a new
one if needed. This performs 1 query against the DB to ensure it's still
valid. When possible (postgres, mssql) that query will be a reset
command to put the connection back into its default state."
  (unless (typep pool 'conn-pool)
    (setf pool (find-or-create-connection-pool connection-spec database-type)))
  (or
   (loop for pconn = (with-process-lock ((conn-pool-lock pool) "Acquire")
		       (pop (free-connections pool)))
	 always pconn
	 thereis
	 ;; test if connection still valid.
	 ;; (e.g. db reboot -> invalid connection )
	 (handler-case
	     (progn (database-acquire-from-conn-pool pconn)
		    pconn)
	   (sql-database-error (e)
	     ;; we could check for a specific error,
	     ;; but, it's safer just to disconnect the pooled conn for any error ?
	     (warn "Database connection ~S had an error while acquiring from the pool:
  ~S
Disconnecting.~%"
		   pconn e)
	     ;;run database disconnect to give chance for cleanup
	     ;;there, then remove it from the lists of connected
	     ;;databases.
	     (%pool-force-disconnect pconn)
	     (with-process-lock ((conn-pool-lock pool) "remove dead conn")
	       (setf (all-connections pool)
		     (delete pconn (all-connections pool))))
	     nil)))
   (let ((conn (connect (connection-spec pool)
			:database-type (pool-database-type pool)
			:if-exists :new
			:make-default nil
                        :encoding encoding)))
     (setf (conn-pool conn) pool)
     (with-process-lock ((conn-pool-lock pool) "new conection")
       (push conn (all-connections pool)))
     conn)))

(defun release-to-pool (database &optional (pool (conn-pool database)))
  "Release a database connection to the pool. The backend will have a
chance to do cleanup."
  (unless (conn-pool database) (setf (conn-pool database) pool))
  (cond
    ;;We read the list of free-connections outside the lock. This
    ;;should be fine as long as that list is never dealt with
    ;;destructively (push and pop destructively modify the place,
    ;;not the list). Multiple threads getting to this test at the
    ;;same time might result in the free-connections getting
    ;;longer... meh.
    ((or (and *db-pool-max-free-connections*
              (>= (length (free-connections pool))
                  *db-pool-max-free-connections*)))
     (%pool-force-disconnect database)

     (with-process-lock ((conn-pool-lock pool) "Remove extra Conn")
       (setf (all-connections pool)
             (delete database (all-connections pool)))))
    (t
     ;;let it do cleanup
     (database-release-to-conn-pool database)
     (with-process-lock ((conn-pool-lock pool) "Release to pool")
       (push database (free-connections pool))))))

(defmethod database-acquire-from-conn-pool (database)
  (case (database-underlying-type database)
    (:postgresql
       (database-execute-command "RESET ALL" database))
    (:mysql
       (database-query "SHOW ERRORS LIMIT 1" database nil nil))
    (:mssql
       ;; rpc escape sequence since this can't be called as a normal sp.
       ;;http://msdn.microsoft.com/en-us/library/aa198358%28SQL.80%29.aspx
       (database-execute-command "{rpc sp_reset_connection}" database))
    (T
       (database-query "SELECT 1;"  database '(integer) nil))))

(defmethod database-release-to-conn-pool (database)
  (case (database-underlying-type database)
    (:postgresql
       (ignore-errors
	 ;;http://www.postgresql.org/docs/current/static/sql-discard.html
	 ;;this was introduced relatively recently, wrap in ignore-errors
	 ;;so that it doesn't choke older versions.
	 (database-execute-command "DISCARD ALL" database)))))

(defun clear-conn-pool (pool)
  "Be careful this function will disconnect connections without regard
to whether another thread is actively using them."
  (with-process-lock ((conn-pool-lock pool) "Clear pool")
    (mapc #'%pool-force-disconnect (all-connections pool))
    (setf (all-connections pool) nil
	  (free-connections pool) nil))
  nil)

(defun find-or-create-connection-pool (connection-spec database-type)
  "Find connection pool in hash table, creates a new connection pool
if not found"
  (let ((key (list connection-spec database-type)))
    (with-process-lock (*db-pool-lock* "Find-or-create connection")
      (or (gethash key *db-pool*)
          (setf (gethash key *db-pool*)
                (make-instance 'conn-pool
                               :connection-spec connection-spec
                               :pool-database-type database-type))))))

(defun disconnect-pooled (&optional clear)
  "Disconnects all connections in the pool. When clear, also deletes
the pool objects."
  (with-process-lock (*db-pool-lock* "Disconnect pooled")
    (maphash
     #'(lambda (key conn-pool)
	 (declare (ignore key))
	 (clear-conn-pool conn-pool))
     *db-pool*)
    (when clear (clrhash *db-pool*)))
  t)

(defun %pool-force-disconnect (database)
  "Force disconnection of a connection from the pool."
  ;;so it isn't just returned to pool
  (setf (conn-pool database) nil)
  ;; disconnect may error if remote side closed connection
  (ignore-errors (disconnect :database database)))

;(defun pool-start-sql-recording (pool &key (types :command))
;  "Start all stream in the pool recording actions of TYPES"
;  (dolist (con (pool-connections pool))
;    (start-sql-recording :type types
;                        :database (connection-database con))))

;(defun pool-stop-sql-recording (pool &key (types :command))
;  "Start all stream in the pool recording actions of TYPES"
;  (dolist (con (pool-connections pool))
;    (stop-sql-recording :type types
;                         :database (connection-database con))))

;(defmacro with-database-connection (pool &body body)
;  `(let ((connection (obtain-connection ,pool))
;         (results nil))
;    (unwind-protect
;         (with-database ((connection-database connection))
;           (setq results (multiple-value-list (progn ,@body))))
;      (release-connection connection))
;    (values-list results)))
