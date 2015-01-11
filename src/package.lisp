(in-package :cl-user)

(defpackage :shpadoinkle
  (:use :cl :iter :alexandria :named-readtables)
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
   #:with-named-labels
   #:=#
   #:cons#
   #:list#
   #:def-conditionalizable-macro
   #:subst-fun))

(in-package :shpadoinkle)

(defparameter *system-directory*
  (make-pathname
   :directory 
   (pathname-directory
    (asdf:system-definition-pathname :shpadoinkle))))
