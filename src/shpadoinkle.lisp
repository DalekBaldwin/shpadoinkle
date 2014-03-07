(in-package :shpadoinkle)

(defmacro with-saved-values (places &body body)
  "Restore values of setf-able places after normal or erroneous exit from body."
  (let ((let-bindings
         `(,@(iter (for place in places)
                   (collect `(,(gensym (format nil "SAVED-~A" place)) ,place))))))
    `(let ,let-bindings
       (unwind-protect
            (progn
              ,@body)
         (setf ,@(iter (for binding in let-bindings)
                       (appending (reverse binding))))))))

(defmacro setup-readtable (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (find-readtable ,name)
       (defreadtable ,name
         (:merge :standard)
         (:dispatch-macro-char #\# #\@
                               ;; #@classname --> (find-class 'classname)
                               (lambda (stream subchar arg)
                                 (declare (ignore subchar arg))
                                 `(find-class (quote ,(read stream t nil t)))))
         (:dispatch-macro-char #\# #\/
                               ;; #/(class-name :slot value) -->
                               ;; (make-instance 'class-name :slot value)
                               (lambda (stream subchar arg)
                                 (declare (ignore subchar arg))
                                 (let ((stuff (read stream t nil t)))
                                   `(make-instance ',(car stuff) ,@(cdr stuff)))))
         (:dispatch-macro-char #\# #\!
                               ;; ignore entire expression, good for commenting out
                               ;; multi-line s-expression
                               (lambda (stream subchar arg)
                                 (declare (ignore subchar arg))
                                 (read stream t nil t)
                                 (values)))
         (:dispatch-macro-char #\# #\`
                               ;; copy-list or copy-tree. for when it's natural
                               ;; to express a list as a quoted form but you
                               ;; can't risk someone else mutating it
                               (lambda (stream subchar arg)
                                 (declare (ignore subchar arg))
                                 `(copy-tree (quote ,(read stream t nil t)))))))))
