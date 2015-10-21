(in-package :cl-user)
(defpackage jonathan.decode
  (:use :cl
        :annot.doc
        :jonathan.error
        :jonathan.util
        :proc-parse)
  (:export :*false-value*
           :*null-value*
           :*empty-object-value*
           :*empty-array-value*
           :parse))
(in-package :jonathan.decode)

(syntax:use-syntax :annot)

@doc
"LISP value of false."
(defvar *false-value* nil)

@doc
"LISP value of null."
(defvar *null-value* nil)

@doc
"LISP value of {}."
(defvar *empty-object-value* nil)

@doc
"LISP value of []."
(defvar *empty-array-value* nil)

(defmacro make-normalizer (keywords)
  (let ((normalizer-block (gensym)))
    `(lambda (key)
       (block ,normalizer-block
         (with-vector-parsing (key)
           (match-case
            ,@(mapcar #'(lambda (key)
                          `(,key (return-from ,normalizer-block ,key)))
                      keywords)
            (otherwise (return-from ,normalizer-block))))))))

(defvar *inner-nest-p* nil)

@doc
"Convert JSON String to LISP object."
(defun parse (string &key (as :plist) junk-allowed keywords-to-read keyword-normalizer normalize-all exclude-normalize-keys)
  (declare (type simple-string string)
           (type (or null function) keyword-normalizer)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let* ((as-alist (eq as :alist))
         (as-jsown (eq as :jsown))
         (as-hash-table (eq as :hash-table))
         (*inner-nest-p* nil))
    (with-vector-parsing (string)
      (macrolet ((with-allowed-last-character ((&optional char &key block (return-value t)) &body body)
                   (let* ((allowed-last-character-block (gensym "allowed-last-character-block")))
                     `(block ,allowed-last-character-block
                        (tagbody
                           (return-from ,allowed-last-character-block
                             (progn ,@body))
                         :eof
                           (or (and junk-allowed (return-from ,(or block allowed-last-character-block) ,return-value))
                               ,(if char
                                    `(if (eq ,char (current))
                                         (return-from ,(or block allowed-last-character-block) ,return-value)
                                         (error '<jonathan-unexpected-eof-error>
                                                :object string))
                                    `(error '<jonathan-unexpected-eof-error> :object string)))))))
                 (skip-spaces ()
                   `(skip* #\Space #\Newline #\Tab))
                 (with-skip-spaces (&body body)
                   `(progn
                      (skip-spaces)
                      (prog1
                          (progn ,@body)
                        (skip-spaces))))
                 (skip?-or-eof (char)
                   `(wIth-allowed-last-character (,char)
                      (or (skip? ,char)
                          (when (eofp) (go :eof)))))
                 (match-and-return (string block value)
                   `(match-case
                     (,string (return-from ,block ,value))
                     (otherwise (or (and junk-allowed (return-from ,block ,value))
                                    (error '<jonathan-incomplete-json-error> :object string)))))
                 (empty-object ()
                   `(if as-hash-table
                        (make-hash-table :test #'equal)
                        *empty-object-value*))
                 (exclude-normalize-key-p (key)
                   `(and exclude-normalize-keys
                         (member ,key exclude-normalize-keys :test #'equal)
                         t)))
        (labels ((dispatch (&optional skip-p force-read-p)
                   (skip-spaces)
                   (match-case
                    ("{" (return-from dispatch (read-object skip-p force-read-p)))
                    ("\"" (return-from dispatch (read-string skip-p)))
                    ("[" (return-from dispatch (read-array skip-p)))
                    ("t" (match-and-return "rue" dispatch t))
                    ("f" (match-and-return "alse" dispatch *false-value*))
                    ("n" (match-and-return "ull" dispatch *null-value*))
                    (otherwise (or (and (integer-char-p (current)) (return-from dispatch (read-number skip-p)))
                                   (error '<jonathan-incomplete-json-error> :object string)))))
                 (read-object (&optional skip-p force-read-p)
                   (skip-spaces)
                   (loop initially (with-allowed-last-character (#\} :block read-object :return-value (empty-object))
                                     (or (and (eofp) (go :eof))
                                         (and (skip? #\}) (return-from read-object (empty-object)))))
                         with result = (when as-hash-table (make-hash-table :test #'equal))
                         as key = (progn (advance*)
                                         (let ((string (read-string skip-p)))
                                           (cond
                                             (skip-p nil)
                                             ((or (not (or keyword-normalizer keywords-to-read))
                                                  force-read-p
                                                  (and (not normalize-all) *inner-nest-p*))
                                              string)
                                             (keyword-normalizer (funcall keyword-normalizer string))
                                             (t (when (member string keywords-to-read :test #'string=) string)))))
                         as value = (and (with-skip-spaces (advance*))
                                         (let ((*inner-nest-p* t))
                                           (dispatch (not key) (exclude-normalize-key-p key))))
                         when key
                           do (cond
                                ((or as-alist as-jsown) (push (cons key value) result))
                                (as-hash-table (setf (gethash key result) value))
                                (t (setq result (nconc (list (make-keyword key) value) result))))
                         until (and (not (with-skip-spaces (skip? #\,)))
                                    (skip?-or-eof #\}))
                         finally (return-from read-object
                                   (if as-jsown
                                       (cons :obj result)
                                       result))))
                 (read-string (&optional skip-p)
                   (let ((start (pos))
                         (escaped-count 0))
                     (declare (type fixnum start escaped-count))
                     (with-allowed-last-character (#\")
                       (skip-while (lambda (c)
                                     (or (and (char= c #\\) (incf escaped-count) (advance*))
                                         (char/= c #\")))))
                     (prog1
                         (unless skip-p
                           (if (= escaped-count 0)
                               (subseq string start (pos))
                               (parse-string-with-escaping start escaped-count)))
                       (advance*))))
                 (parse-string-with-escaping (start escaped-count)
                   (declare (type fixnum start escaped-count))
                   (loop with result = (make-string (- (pos) start escaped-count))
                         with result-index = 0
                         with escaped-p
                         for index from start below (pos)
                         for char = (char string index)
                         if escaped-p
                           do (setf escaped-p nil)
                              (setf (char result result-index)
                                    (case char
                                      (#\b #\Backspace)
                                      (#\f #\Linefeed)
                                      (#\n #\Linefeed)
                                      (#\r #\Return)
                                      (#\t #\Tab)
                                      (t char)))
                              (incf result-index)
                              (when (zerop (decf escaped-count))
                                (return-from parse-string-with-escaping
                                  (replace result (the (simple-array character (*)) (subseq string (1+ index)))
                                           :start1 result-index)))
                         else
                           if (char= char #\\)
                             do (setf escaped-p t)
                         else
                           do (setf (char result result-index) char)
                              (incf result-index)
                         finally (return result)))
                 (read-array (&optional skip-p)
                   (skip-spaces)
                   (or (loop until (skip?-or-eof #\])
                             collect (dispatch skip-p)
                             do (with-skip-spaces (skip? #\,)))
                       *empty-array-value*))
                 (read-number (&optional skip-p)
                   (if skip-p
                       (tagbody
                          (skip-while integer-char-p)
                          (when (skip? #\.)
                            (skip-while integer-char-p))
                        :eof
                          (return-from read-number))
                       (bind (num-str (skip-while integer-char-p))
                         (let ((num (the fixnum (parse-integer num-str))))
                           (when (with-allowed-last-character ()
                                   (skip? #\.))
                             (setq num
                                   (block nil
                                     (let ((rest-start (the fixnum (pos))))
                                       (bind (rest-num-str (skip-while integer-char-p))
                                         (let ((rest-num (the fixnum (parse-integer rest-num-str))))
                                           (return (+ num  (float (/ rest-num (the fixnum (expt 10 (- (pos) rest-start)))))))))))))
                           (when (with-allowed-last-character ()
                                   (skip? #\e #\E))
                             (setq num
                                   (block nil
                                     (bind (exp-num-str (skip-while (lambda (char) (or (integer-char-p char)
                                                                                       (char= char #\+)))))
                                       (let ((exp-num (the fixnum (parse-integer exp-num-str))))
                                         (return (* num
                                                    (if (< exp-num 0)
                                                        (float (expt 10 exp-num))
                                                        (expt 10 exp-num)))))))))
                           (return-from read-number (the fixnum num)))))))
          (declare (inline read-object read-string parse-string-with-escaping read-array read-number))
          (skip-spaces)
          (return-from parse (dispatch)))))))


(define-compiler-macro parse (&whole form string &key (as :plist) junk-allowed keywords-to-read
                                     keyword-normalizer normalize-all exclude-normalize-keys)
  (handler-case
      (if (and (not keyword-normalizer)
               (foldable-keywords-to-read-p keywords-to-read))
          (let ((keywords (eval keywords-to-read)))
            `(parse ,string :as ,as
                            :junk-allowed ,junk-allowed
                            :keywords-to-read ,keywords-to-read
                            :keyword-normalizer (make-normalizer ,keywords)
                            :normalize-all ,normalize-all
                            :exclude-normalize-keys ,exclude-normalize-keys))
          form)
    (error () form)))

(defun foldable-keywords-to-read-p (keywords-to-read)
  (and keywords-to-read
       (consp keywords-to-read)
       (case (car keywords-to-read)
         (quote (every #'stringp (cadr keywords-to-read)))
         (list (every #'stringp (cdr keywords-to-read))))))
