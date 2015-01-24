;;;; shpadoinkle.asd

(defpackage #:shpadoinkle-system
  (:use #:cl #:asdf))
(in-package #:shpadoinkle-system)

(defsystem #:shpadoinkle
  :name "shpadoinkle"
  :serial t
  :components
  ((:static-file "shpadoinkle.asd")
   (:module :src
            :components ((:file "package")
                         (:file "shpadoinkle")
                         (:file "with-named-labels"))
            :serial t))
  :depends-on (#:iterate #:alexandria #:named-readtables))

(defsystem #:shpadoinkle-test
  :name "shpadoinkle-test"
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "shpadoinkle-test"))
            :serial t))
  :depends-on (#:shpadoinkle #:stefil #:lisp-unit))
