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
                          `(,var ,(cdr (assoc var gensyms))))))
            (var-list (,@(iter (for binding in bindings)
                               (collect (first binding))))))
         (macrolet
             ((local-drelet (dbindings &body body &environment env)
                (let ((dbindings (iter (for dbinding in dbindings)
                                       (collect
                                           (if (atom dbinding) (list dbinding) dbinding))))
                      (var-list (macroexpand 'var-list env)))
                  `(let (,@(iter (for dbinding in dbindings)
                                 (collect
                                     (let ((var (first dbinding)))
                                       (unless (member var var-list)
                                         (error "~A not declared with LOCAL-DLET" var))
                                       `(,(macroexpand var env) ,(second dbinding))))))
                     ,@body))))
           ,@body)))))

(defmacro with-named-labels (names expression &body body)
  "Create a data structure with possibly multiple references to objects.
Like the #n# read macro, this allows the visual structure of the code to mirror
the shape of the final data structure, but the labels used to refer to parts of
the structure are also used as variable names and are bound to the corresponding
parts of the structure within the macro body. Also the object, and the topology
of its internal references, need not be constant. Requires the use of special
variants of primitive data constructors."
  `(let* (;; pre-bind names to unique dummy objects
          ,@(loop for name in names
               collect `(,name (list :a-dummy)))
          ;; create a mapping from those dummies to symbols
          (*dummy->name* (list ,@(loop for name in names collect
                                      `(cons ,name ',name))))
          ;; and a mapping from symbols to final objects, to be filled in later
          (*name->object*)
          ;; info about what pointers need to be reassigned later
          (*targets*))
     ;; construct skeleton
     ,expression
     ;; replace dummy references
     (replace-dummies)
     ;; bind actual variable names
     (let (,@(loop for name in names
                collect `(,name (lookup ',name *name->object*))))
       (declare (ignorable ,@names))
       ;; and make objects accessible through those names in the body
       ,@body)))

;;;; Auxiliary definitions for WITH-NAMED-LABELS

(defvar *dummy->name*)
(defvar *name->object*)
(defvar *targets*)

(defun lookup (key alist)
  (cdr (assoc key alist)))

(defun cons# (head tail)
  "Variant of CONS for use in WITH-NAMED-LABELS to allow references to internal labels."
  (let ((cons-cell (cons head tail)))
    (when (assoc head *dummy->name*)
      (push (cons cons-cell :car) *targets*))
    (when (assoc tail *dummy->name*)
      (push (cons cons-cell :cdr) *targets*))
    cons-cell))

(defun list# (&rest args)
  "Variant of LIST for use in WITH-NAMED-LABELS to allow references to internal labels."
  (let ((the-list (apply #'list args)))
    (iter (for arg in args)
          (for i from 0)
          (when (assoc arg *dummy->name*)
            (push (cons the-list i) *targets*)))
    the-list))

(defmacro =# (name expression)
  "Establish a label for use within a WITH-NAMED-LABELS construction form."
  (let ((object (gensym "OBJECT")))
    `(let ((,object ,expression))
       (push (cons ',name ,object) *name->object*)
       ,object)))

(defun replace-dummies ()
  (iter (for target in *targets*)
        (let ((structure (car target))
              (pointer (cdr target)))
          (case pointer
            ((:car) (setf (car structure)
                          (lookup
                           (lookup (car structure) *dummy->name*)
                           *name->object*)))
            ((:cdr) (setf (cdr structure)
                          (lookup
                           (lookup (cdr structure) *dummy->name*)
                           *name->object*)))
            (t (setf (elt structure pointer)
                     (lookup
                      (lookup (elt structure pointer) *dummy->name*)
                      *name->object*)))))))

(defmacro def-conditionalizable-macro (name lambda-list &rest body)
  "Define a macro in which unevaluated subforms may contain conditionals.
The test is hoisted to beginning of the expansion and the macro proceeds as if
the resulting so-called branch had appeared as a single form in the position
where the conditional appears. For example, if we defined SETF using this macro,
`(setf (if (test) x y) z)` could expand into `(if (test) (setq x z) (setq y z)`."
  (let ((processed (gensym "PROCESSED"))
        (remaining (gensym "REMAINING"))
        (expression (gensym "EXPRESSION"))
        (clause (gensym "CLAUSE"))
        (case (gensym "CASE")))
    `(defmacro ,name ,lambda-list
       (labels
           ((extract-conditionals (,processed ,remaining)
              (cond
                ((null ,remaining)
                 (destructuring-bind ,lambda-list (reverse ,processed)
                   ,@body))
                ((atom (first ,remaining))
                 (extract-conditionals
                  (cons (first ,remaining) ,processed)
                  (rest ,remaining)))
                (t
                 (let ((,expression (first ,remaining)))
                   (case (first ,expression)
                     (if `(if ,(second ,expression)
                              ,(extract-conditionals
                                (cons (third ,expression) ,processed)
                                (rest ,remaining))
                              ,(extract-conditionals
                                (cons (fourth ,expression) ,processed)
                                (rest ,remaining))))
                     (cond `(cond
                              ,@(iter (for ,clause in (rest ,expression))
                                      (collect `(,(first ,clause)
                                                  ,(extract-conditionals
                                                    (cons (second ,clause) ,processed)
                                                    (rest ,remaining)))))))
                     (case `(case ,(second ,expression)
                              ,@(iter (for ,case in (nthcdr 2 ,expression))
                                      (collect `(,(first ,case)
                                                  ,(extract-conditionals
                                                    (cons (second ,case) ,processed)
                                                    (rest ,remaining)))))))
                     (otherwise (extract-conditionals
                                 (cons ,expression ,processed)
                                 (rest ,remaining)))))))))
         (extract-conditionals nil (list ,@lambda-list))))))
