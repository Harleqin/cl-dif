;;;; package.lisp

(defpackage #:cl-dif
  (:use #:cl
        #:alexandria
        #:split-sequence
        #:parse-number)
  (:export #:read-dif
           #:write-dif
           #:dif-ref
           #:make-dif
           #:map-dif-column
           #:map-into-dif-tuples))

