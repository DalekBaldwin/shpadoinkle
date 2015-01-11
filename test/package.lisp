(in-package :cl-user)

(defpackage :shpadoinkle-test
  (:use :cl :shpadoinkle :stefil :lisp-unit)
  (:export
   #:run-all-tests))

(in-package :shpadoinkle-test)

(defparameter *system-directory*
  (make-pathname
   :directory 
   (pathname-directory
    (asdf:system-definition-pathname :shpadoinkle))))
