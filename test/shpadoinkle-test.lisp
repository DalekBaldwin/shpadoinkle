(in-package #:shpadoinkle-test)

(in-root-suite)

(defsuite* test-all)

(defun run-all-tests ()
  (format t " |~%") ;; "|" needed to avoid screwing up colors in SLIME REPL
  (test-all)
  (let ((results
         (run-tests :all :shpadoinkle-test)))
    (print-errors results)
    (print-failures results)))

(define-test test-with-saved-values-expansion
  (assert-expands
   (LET ((#:SAVED-HERP1 HERP)
         (#:SAVED-*DERP*2 *DERP*)
         (#:|SAVED-(CAR NERP)3| (CAR NERP))
         (#:|SAVED-(GETHASH 'BARF NARF)4| (GETHASH 'BARF NARF)))
     (UNWIND-PROTECT (PROGN (INCF HERP) (SETF *DERP* (CAAR NERP)) (ERROR "barfed"))
       (SETF HERP #:SAVED-HERP1
             *DERP* #:SAVED-*DERP*2
             (CAR NERP) #:|SAVED-(CAR NERP)3|
             (GETHASH 'BARF NARF) #:|SAVED-(GETHASH 'BARF NARF)4|)))
   (with-saved-values (herp *derp* (car nerp) (gethash 'barf narf))
     (incf herp)
     (setf *derp* (caar nerp))
     (error "barfed"))))

(deftest test-with-named-labels ()
  (with-named-labels (a)
      (=# a (cons 1 (cons# 2 a))) ;; #1=(1 2 . #1#)
    (is (eql a (cddr a))))
  (with-named-labels (b)
      (=# b (list# 1 2 b)) ;; #1=(1 2 #1#)
    (is (eql b (third b))))
  (with-named-labels (c d)
      (=# c (list# 1
                   2
                   (if (< 2 3) c d)
                   (=# d (list# 3 4)))) ;; #1=(1 2 #1# (3 4))
    (is (eql c (third c)))))
