(in-package #:shpadoinkle)

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
