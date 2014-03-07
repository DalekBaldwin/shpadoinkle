(in-package :cl)

(defpackage :shpadoinkle
  (:use :cl :iter :named-readtables)
  (:export
   #:with-saved-values
   #:setup-readtable
   #:in-readtable))

(in-package :shpadoinkle)

(defparameter *system-directory*
  (make-pathname
   :directory 
   (slot-value
    (asdf:system-definition-pathname :shpadoinkle)
    'directory)))
