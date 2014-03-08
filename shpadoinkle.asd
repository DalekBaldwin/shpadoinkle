;;;; shpadoinkle.asd

(defpackage :shpadoinkle-system
  (:use :cl :asdf))
(in-package :shpadoinkle-system)

(defsystem :shpadoinkle
  :name "shpadoinkle"
  :serial t
  :components
  ((:static-file "shpadoinkle.asd")
   (:module :src
            :components ((:file "package")
                         (:file "shpadoinkle" :depends-on ("package")))))
  :depends-on (:iterate :alexandria :named-readtables))

(defsystem :shpadoinkle-test
  :name "shpadoinkle-test"
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "shpadoinkle-test" :depends-on ("package")))))
  :depends-on (:shpadoinkle :stefil :lisp-unit))
