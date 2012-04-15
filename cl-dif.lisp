;;;; cl-dif.lisp

(in-package #:cl-dif)

(defclass dif ()
  ((name :accessor dif-name
         :initarg :name
         :initform "")
   (version :accessor dif-version
            :initarg :version
            :initform 0)
   (vectors :accessor dif-vectors       ; columns
            :initarg :vectors
            :initform 0)
   (tuples :accessor dif-tuples         ; rows
           :initarg :tuples
           :initform 0)
   (contents :accessor dif-contents
             :initarg :contents
             :initform (make-array '(0 0))))
  (:documentation
   "Representation of a DIF (data interchange format) file.  Vectors
and tuples represent the number of columns and rows.  The contents are
held in a two-dimensional array.  When the size of the table is
modified, a new array is allocated and the contents copied."))

(defun in-scope-p (dif row col)
  (and (<= 0 row)
       (< row (dif-tuples dif))
       (<= 0 col)
       (< col (dif-vectors dif))))

(defun copy-array-contents (from-array to-array)
  "Copies the contents of from-array to to-array and returns to-array.
Both arrays must be two-dimensional, and to-array must be at least as
big in both dimensions as from-array."
  (destructuring-bind (rows cols) (array-dimensions from-array)
    (dotimes (r rows)
      (dotimes (c cols)
        (setf (aref to-array r c)
              (aref from-array r c))))
    to-array))

(defun dif-ref (dif row col)
  "Returns the value at the given row and column.  If this coordinate
is out of the scope of the dif, return :na."
  (if (in-scope-p dif row col)
      (aref (dif-contents dif) row col)
      :na))

(defun (setf dif-ref) (dif row col value)
  "Sets the value at the coordinate given by row and col of the dif to
the given value.  If this coordinate is out of the current scope of
the dif, first expands the scope of the dif by allocating a new array
and copying the values over."
  (cond ((in-scope-p dif row col)
         (setf (aref (dif-contents dif) row col) value))
        ((or (minusp row) (minusp col))
         (error "negative array index: (~a ~a)" row col))
        (t
         (let* ((new-rows (max (1+ row)
                               (dif-tuples dif)))
                (new-cols (max (1+ col)
                               (dif-vectors dif)))
                (new-contents (make-array (list new-rows new-cols))))
           (copy-array-contents (dif-contents dif) new-contents)
           (setf (dif-contents dif) new-contents
                 (dif-tuples dif) new-rows
                 (dif-vectors dif) new-cols
                 (aref (dif-contents dif) row col) value)))))

(defun map-dif-column (function dif column &key (start-row 0) (end-row (dif-tuples dif)))
  (loop :for row :from start-row :below end-row
        :collect (funcall function (dif-ref dif row column))))

(defun map-into-dif-tuples (function dif &key (start-row 0) (end-row (dif-tuples dif)))
  "Calls the unary function function with an array displaced to the
dif contents of each row. Function can thus modify these contents."
  (loop :for row :from start-row :below end-row
        :do (let ((tuple (make-array (dif-vectors dif)
                                     :displaced-to (dif-contents dif)
                                     :displaced-index-offset (* row (dif-vectors dif)))))
              (funcall function tuple))))

(defun make-dif (name columns rows)
  (make-instance 'dif
                 :name name
                 :version 1
                 :vectors columns
                 :tuples rows
                 :contents (make-array (list rows columns)
                                       :initial-element :na)))

(defun copy-dif (dif)
  (let ((copy (make-dif (dif-name dif)
                        (dif-vectors dif)
                        (dif-tuples dif))))
    (setf (dif-contents copy)
          (copy-array (dif-contents dif)))))

(defun quoted-string-p (string)
  (char= #\"
         (aref string 0)
         (aref string (1- (length string)))))

(defun to-dif-string (string)
  "Given a string in double quotes (first and last character are
'\"'), returns the contents, but replaces all occurrences of two
consecutive '\"' with a single '\"'."
  (cl-ppcre:regex-replace-all "\"\""
                              (subseq string 1 (1- (length string)))
                              "\""))

(defmacro with-read-dif-values ((type n-val s-val) stream &body body)
  (with-gensyms (line0 line1 type-string n-val-string)
    `(let* ((,line0 (read-line ,stream))
            (,line1 (read-line ,stream)))
       (destructuring-bind (,type-string ,n-val-string) (split-sequence #\, ,line0)
         (let ((,type (parse-integer ,type-string))
               (,n-val (parse-number ,n-val-string))
               (,s-val (if (quoted-string-p ,line1)
                           (to-dif-string ,line1)
                           (intern (string-upcase ,line1) "KEYWORD"))))
           ,@body)))))

(defconstant +dif-number+ 0)

(defconstant +dif-string+ 1)

(defconstant +dif-keyword+ -1)

(defun read-dif-datum (stream)
  (with-read-dif-values (type n-val s-val) stream
    (ecase type
      (0 (case s-val
           (:v n-val)
           (:na :na)
           (:error :error)
           (:true t)
           (:false nil)
           (t n-val)))
      ((1 -1) s-val))))

(defun read-data-into-dif (stream dif)
  (let ((row -1)
        (col 0))
    (loop :for val := (read-dif-datum stream)
          :do (case val
                (nil )
                (:bot (incf row)
                      (setf col 0))
                (:eod (return-from read-data-into-dif dif))
                (t (setf (dif-ref dif row col) val)
                   (incf col))))))

(defun read-dif (filespec)
  "Loads a dif-spreadsheet from the given dif file.  The VECTORS and
TUPLES values from the file are taken as hints for preallocating the
array that holds the values."
  (with-open-file (in filespec
                      :direction :input)
    (let ((result-dif nil))
      (loop :for token := (read-line in nil)
            :while token
            :do (ecase token
                  ("TABLE"
                   (with-read-dif-values (type n-val s-val) in
                     (declare (ignore type))
                     (setf result-dif
                           (make-instance 'dif
                                          :name s-val
                                          :version n-val))))
                  ("VECTORS"
                   (with-read-dif-values (type n-val s-val) in
                     (declare (ignore type s-val))
                     (setf (dif-vectors result-dif) n-val)))
                  ("TUPLES"
                   (with-read-dif-values (type n-val s-val) in
                     (declare (ignore type s-val))
                     (setf (dif-tuples result-dif) n-val)))
                  ("DATA"
                   (with-read-dif-values (type n-val s-val) in
                     (declare (ignore type n-val s-val)))
                   (setf (dif-contents result-dif)
                         (make-array (list (dif-tuples result-dif)
                                           (dif-vectors result-dif))))
                   (read-data-into-dif in result-dif)
                   (return-from read-dif result-dif)))))))

(defun write-dif-val (value stream)
  (case value
    (:na (format stream "0,0~%NA~%"))
    (:error (format stream "0,0~%ERROR~%"))
    ((t) (format stream "0,0~%TRUE~%"))
    ((nil) (format stream "0,0~%FALSE~%"))
    (t
     (etypecase value
       (number (format stream "0,~a~%V~%"
                       value))
       (string (format stream "1,0~%~s~%"
                       (cl-ppcre:regex-replace-all "\""
                                                   value
                                                   "\"\"")))))))

(defun write-dif (dif filespec &key (external-format :utf8))
  "Writes a dif-spreadsheet to the given file."
  (with-open-file (out filespec
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :external-format external-format)
    (format out "TABLE~%0,~a~%~s~%"
            (dif-version dif) (dif-name dif))
    (format out "VECTORS~%0,~a~%\"\"~%"
            (dif-vectors dif))
    (format out "TUPLES~%0,~a~%\"\"~%"
            (dif-tuples dif))
    (format out "DATA~%0,0~%\"\"~%")
    (dotimes (row (dif-tuples dif))
      (format out "-1,0~%BOT~%")
      (dotimes (col (dif-vectors dif))
        (write-dif-val (dif-ref dif row col) out)))
    (format out "-1,0~%EOD~%")))
