(in-package :cl)

(defpackage :shpadoinkle
  (:use :cl :iter :alexandria :named-readtables :macroexpand-dammit)
  (:export
   #:with-saved-values
   #:with-constructors
   #:setup-readtable
   
   ;; I like not having to make named-readtables an explicit dependency
   #:in-readtable
   #:if-not-let
   #:substitute-nth
   #:partial
   #:partial-pattern
   #:with-homonymous-accessors
   #:local-dlet
   #:local-drelet
   #:with-names
   #:label
   #:label-cons
   #:label-list))

(in-package :shpadoinkle)

(defparameter *system-directory*
  (make-pathname
   :directory 
   (slot-value
    (asdf:system-definition-pathname :shpadoinkle)
    'directory)))
