(in-package :cl)

(defpackage :shpadoinkle
  (:use :cl :iter :alexandria :named-readtables)
  (:export
   #:with-saved-values
   #:with-constructors
   #:setup-readtable
   #:in-readtable))

(in-package :shpadoinkle)

(defparameter *system-directory*
  (make-pathname
   :directory 
   (slot-value
    (asdf:system-definition-pathname :shpadoinkle)
    'directory)))
