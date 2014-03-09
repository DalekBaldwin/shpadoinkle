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

(defmacro with-constructors (names &body body)
  "Treat class names like functions to call `make-instance`.
   Helps clean up some code, but it's probably dangerous. Use the #/ read-macro
   instead."
  (with-gensyms (initargs)
    `(macrolet
         (,@(iter (for name in names)
                  (collect `(,name (&rest ,initargs)
                                   `(make-instance ',',name ,@,initargs)))))
       ,@body)))

(defmacro setup-readtable (name)
  "Some read-macros I find convenient.
   In Programmer Dvorak, they can all be typed with two adjacent keys. Or one
   key, in the case of #`."
  (with-gensyms (stream subchar arg stuff))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (find-readtable ,name)
       (defreadtable ,name
         (:merge :standard)
         
         ;; #@classname --> (find-class 'classname)
         (:dispatch-macro-char #\# #\@
                               (lambda (,stream ,subchar ,arg)
                                 (declare (ignore ,subchar ,arg))
                                 `(find-class (quote ,(read ,stream t nil t)))))
         
         ;; #/(class-name :slot value) -->
         ;; (make-instance 'class-name :slot value)
         (:dispatch-macro-char #\# #\/
                               (lambda (,stream ,subchar ,arg)
                                 (declare (ignore ,subchar ,arg))
                                 (let ((,stuff (read ,stream t nil t)))
                                   `(make-instance ',(car ,stuff) ,@(cdr ,stuff)))))
         
         ;; Ignore entire form. Works just like `#+nil`.
         ;; Good for commenting out multi-line s-expressions. The
         ;; reader will still parse the form and complain about things
         ;; before throwing it away, but I haven't yet bothered to
         ;; figure out how to stop that.  Maybe it's for the best, so
         ;; you don't leave so much commented-out code hanging around
         ;; referring to things that haven't existed since twenty
         ;; commits ago.
         (:dispatch-macro-char #\# #\!
                               (lambda (,stream ,subchar ,arg)
                                 (declare (ignore ,subchar ,arg))
                                 (read stream t nil t)
                                 (values)))
         
         ;; (form) --> (copy-tree '(form))
         ;; For when it's natural to express a list as a quoted form
         ;; but you can't risk someone else mutating it, since
         ;; implementations typically store quoted forms as constants.
         (:dispatch-macro-char #\# #\`
                               (lambda (,stream ,subchar ,arg)
                                 (declare (ignore ,subchar ,arg))
                                 `(copy-tree (quote ,(read ,stream t nil t)))))))))

(defmacro if-not-let (bindings &body (then-form &optional else-form))
  "Like `if-let` for cases when it's more natural to express the else clause first."
  `(if-let ,bindings
     ,else-form
     ,then-form))

(defun substitute-nth (n list value)
  "Return a new version of `list` with `value` at position `n`.
   There's probably a more efficient way to do this."
  (append (subseq list 0 n)
          (cons value (subseq list (1+ n)))))
