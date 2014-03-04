(in-package :cl)

(defpackage :shpadoinkle
  (:use :cl :iter)
  (:export
   #:with-saved-values))

(in-package :shpadoinkle)

(defparameter *system-directory*
  (make-pathname
   :directory 
   (slot-value
    (asdf:system-definition-pathname :shpadoinkle)
    'directory)))
