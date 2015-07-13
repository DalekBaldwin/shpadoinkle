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

(defun subst-fun (new old tree &key key (test #'eql testp) (test-not #'eql notp))
  "A higher-order version of SUBST, useful for simple code transformations.
Instead of passing a single element to replace all matching nodes in a TREE,
NEW is a function that takes the existing node as input. Based on SBCL's
implementation of SUBST."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (labels ((s (subtree)
             (cond ((let ((key-tmp (if key
                                       (funcall key subtree)
                                       subtree)))
                      (cond (testp (funcall test old key-tmp))
                            (notp (not (funcall test-not old key-tmp)))
                            (t (funcall test old key-tmp))))
                    (funcall new subtree))
                   ((atom subtree)
                    subtree)
                   (t (let ((car (s (car subtree)))
                            (cdr (s (cdr subtree))))
                        (if (and (eq car (car subtree))
                                 (eq cdr (cdr subtree)))
                            subtree
                            (cons car cdr)))))))
    (s tree)))

(defun format-exact-decimal (stream arg colon at &optional (power 0))
  "Print integer shifted by number of decimal places indicated in first param.
This is necessary for printing large exact currency amounts because using the ~$
format directive on rationals may cause rounding."
  (declare
   (ignore colon at)
   (type integer arg))
  (let* ((scale (expt 10 power)))
    (multiple-value-bind (whole fraction) (floor (/ arg scale))
      (format stream "~D.~V,'0D" whole power (* scale fraction)))))

(defmacro destructuring-lambda (params &body body)
  (let ((shallow-params (gensym "SHALLOW-PARAMS")))
    `(lambda (&rest ,shallow-params)
       (destructuring-bind (,params) ,shallow-params
         ,@body))))

(defmacro def-dynkey-fun (name args &body body)
  "Define a function with the additional lambda list directive &dynkey.
Symbols listed after &dynkey can be used to optionally establish new
bindings for special variables."
  (create-dynkey-fun-form name args body))

(defun separate-dynkeys (args)
  (let ((in-dynkeys nil))
    (destructuring-bind (reverse-standard reverse-dynkeys)
        (reduce
         (lambda (accum new)
           (cond
             (in-dynkeys
              (when (not (and (symbolp new)
                              (not (keywordp new))))
                (error "Dynkey ~A is not a non-keyword symbol in lambda list: ~A"
                       new args))
              (when (member new (list '&optional '&key '&rest))
                (error "Misplaced ~A in dynkey lambda list: ~A"
                       new args))
              (destructuring-bind (standard dynkeys) accum
                (cond ((member new (list '&allow-other-keys '&aux))
                       (setf in-dynkeys nil)
                       (list (list* new standard) dynkeys))
                      (t
                       (list standard (list* new dynkeys))))))
             (t
              (destructuring-bind (standard dynkeys) accum
                (cond
                  ((eql new '&dynkey)
                   (setf in-dynkeys t)
                   accum)
                  (t
                   (list (list* new standard) dynkeys)))))))
         args
         :initial-value (list nil nil))
      (values (reverse reverse-standard)
              (reverse reverse-dynkeys)))))

(defun create-dynkey-fun-form (name args body)
  (multiple-value-bind (standard dynkeys) (separate-dynkeys args)
    (multiple-value-bind (required optional rest keys allow-other-keys aux)
        (parse-ordinary-lambda-list standard :normalize nil)
      (let ((new-keys (append (mapcar (lambda (sym) `(,sym ,sym)) dynkeys)
                              keys)))
        (multiple-value-bind (code-body decls doc)
            (parse-body body :documentation t)
          `(defun ,name (,@required
                         ,@(when optional `(&optional ,@optional))
                                              ,@(when rest `(&rest ,rest))
                         &key ,@new-keys
                           ,@(when allow-other-keys `(&allow-other-keys))
                           ,@(when aux `(&aux ,@aux)))
             ,@(when doc (list doc))
             (declare (special ,@dynkeys))
             ,@decls
             ,@code-body))))))

(defun gen-vars (size prefix)
  (loop for i below size
     collect (make-symbol (format nil "~A-~A" prefix i))))

(defun partially-ordered-p (predicate list)
  (cond
    ((endp list)
     t)
    ((endp (rest list))
     (valuest t (first list)))
    (t
     (values t
             (reduce (lambda (prev-item new-item)
                       (if (funcall predicate prev-item new-item)
                           new-item
                           (return-from partially-ordered-p nil)))
                     (rest list) :initial-value (first list))))))

(defmacro equal-values (&rest forms)
  `(partially-ordered-p
    #'equal
    (list ,@(loop for form in forms collect `(multiple-value-list ,form)))))
