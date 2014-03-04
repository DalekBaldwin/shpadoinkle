(in-package :shpadoinkle-test)

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
