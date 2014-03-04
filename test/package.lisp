(in-package :cl)

(defpackage :shpadoinkle-test
  (:use :cl :shpadoinkle :stefil :lisp-unit)
  (:export
   #:run-all-tests))

(in-package :shpadoinkle-test)

(defparameter *system-directory*
  (make-pathname
   :directory 
   (slot-value
    (asdf:system-definition-pathname :shpadoinkle)
    'directory)))
