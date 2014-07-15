(in-package :shpadoinkle)

(defmacro with-saved-values (places &body body)
  "Restore values of setf-able PLACES after normal or erroneous exit from BODY."
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
  "Treat class NAMES like functions to call MAKE-INSTANCE.
This can make tests and example code shorter and clearer, but it's probably too
dangerous for actually defining new functionality. Use the #/ read-macro
instead."
  (with-gensyms (initargs)
    `(macrolet
         (,@(iter (for name in names)
                  (collect `(,name (&rest ,initargs)
                                   `(make-instance ',',name ,@,initargs)))))
       ,@body)))

(defmacro setup-readtable (name)
  "Some read-macros I find convenient.
In Programmer Dvorak, they can all be typed with two adjacent keys. Or one key,
in the case of #\# #\`."
  (with-gensyms (stream subchar arg stuff)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless (find-readtable ,name)
         (defreadtable ,name
           (:merge :standard)
           
           ;; #@classname --> (find-class 'classname)
           (:dispatch-macro-char
            #\# #\@
            (lambda (,stream ,subchar ,arg)
              (declare (ignore ,subchar ,arg))
              `(find-class (quote ,(read ,stream t nil t)))))
           
           ;; #/(class-name :slot value) --> (make-instance 'class-name :slot value)
           (:dispatch-macro-char
            #\# #\/
            (lambda (,stream ,subchar ,arg)
              (declare (ignore ,subchar ,arg))
              (let ((,stuff (read ,stream t nil t)))
                `(make-instance ',(car ,stuff) ,@(cdr ,stuff)))))
           
           ;; Ignore entire form. Works just like #+nil.
           ;; This will essentially comment out a whole s-expression even if it
           ;; spans multiple lines. The reader will still parse the form and give
           ;; you warnings about stuff before throwing it away, but I haven't yet
           ;; bothered to figure out how to stop that.  Maybe it's for the best,
           ;; because it helps nudge you to delete old commented-out code
           ;; referring to things that haven't existed since twenty commits
           ;; ago. Note that if you place two of these in a row, like:
           ;; `#! #! (form) (form)`
           ;; they will ignore two forms in a row, but this same behavior happens
           ;; if you place two #+ tests in a row that fail, so I don't consider it
           ;; a bug.
           (:dispatch-macro-char
            #\# #\!
            (lambda (,stream ,subchar ,arg)
              (declare (ignore ,subchar ,arg))
              (read ,stream t nil t)
              (values)))
           
           ;; #`(form) --> (copy-tree '(form))
           ;; For when it's natural to express a list as a quoted form but you
           ;; can't risk someone else mutating it, since implementations typically
           ;; store quoted forms as constants.
           (:dispatch-macro-char
            #\# #\`
            (lambda (,stream ,subchar ,arg)
              (declare (ignore ,subchar ,arg))
              `(copy-tree (quote ,(read ,stream t nil t))))))))))

(defmacro if-not-let (bindings &body (else-form &optional then-form))
  "Like IF-LET for cases when it's more natural to express the else clause first."
  `(if-let ,bindings
     ,then-form
     ,else-form))

(defun substitute-nth (n list value)
  "Return a new version of LIST with VALUE at position N.
There's probably a more efficient way to do this."
  (append (subseq list 0 n)
          (cons value (subseq list (1+ n)))))

(defmacro partial (function &rest args)
  "Return a partially-applied version of FUNCTION with ARGS applied to the
earliest parameters in the lambda list."
  (with-gensyms (remaining-params)
    `(lambda (&rest ,remaining-params)
       (apply ,function ,@args ,remaining-params))))

(defmacro partial-pattern (function blank partial-arg-pattern)
  "Return a partially-applied function with the remaining arguments to be
inserted at positions in the lambda list corresponding to appearances of the
symbol BLANK."
  (let ((remaining-params
         (iter (for arg in partial-arg-pattern)
               (when (eql arg blank)
                 (collect (gensym)))))
        (blank-position 0))
    `(lambda ,remaining-params
       (,function
        ,@(iter (for arg in partial-arg-pattern)
                (if (eql arg blank)
                    (prog1
                        (collect (elt remaining-params blank-position))
                      (incf blank-position))
                    (collect arg)))))))

(defmacro with-homonymous-accessors (accessors instance &body body)
  "Like WITH-ACCESSORS but with the brevity of WITH-SLOTS."
  `(with-accessors
         (,@(iter (for accessor in accessors)
                  (collect `(,accessor ,accessor))))
       ,instance
     ,@body))

(defmacro local-dlet (bindings &body body)
  "Establish top-level lexically-scoped bindings that can be dynamically
   shadowed with LOCAL-DRELET. A sort of FLUID-LET facility that uses LET
   underneath and therefore can create thread-local bindings."
  (let* ((bindings (iter (for binding in bindings)
                         (collect (if (atom binding) (list binding) binding))))
         (gensyms
          (iter (for binding in bindings)
                (collect (let ((var (first binding)))
                           (cons var (gensym (symbol-name var))))))))
    `(progn
       ,@(iter (for binding in bindings)
               (collect
                   `(defvar ,(cdr (assoc (first binding) gensyms)) ,@(rest binding))))
       (symbol-macrolet
           (,@(iter (for binding in bindings)
                    (collect
                        (let ((var (first binding)))
                          `(,var ,(cdr (assoc var gensyms)))))))
         (macrolet ((local-drelet (dbindings &body body &environment env)
                      (let ((dbindings (iter (for dbinding in dbindings)
                                             (collect
                                                 (if (atom dbinding) (list dbinding) dbinding)))))
                        `(let (,@(iter (for dbinding in dbindings)
                                       (collect
                                           (let ((var (first dbinding)))
                                             `(,(macroexpand var env) ,(second dbinding))))))
                           ,@body))))
           ,@body)))))
