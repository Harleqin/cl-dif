;;;; cl-dif.asd

(asdf:defsystem #:cl-dif
  :serial t
  :depends-on (#:alexandria
               #:split-sequence
               #:parse-number
               #:cl-ppcre)
  :components ((:file "package")
               (:file "cl-dif")))

