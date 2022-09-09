#lang eopl

;; top level module.  Loads all required pieces.
;; Run the test suite with (run-all).

;;; interface for book test ;;;
(provide test-all)
(define (test-all)
  (run-all))

(require "drscheme-init.rkt")
(require "data-structures.rkt")        ; for expval constructors
(require "cps-in-lang.rkt")            ; for scan&parse
(require "cps.rkt")                    ; for cps transformer
(require "interp.rkt")                 ; for value-of-program
(require "tests.rkt")                  ; for test-list

(require "cps-out-lang.rkt")              ; for cps-program->string

(provide (all-defined-out))
(provide (all-from-out "interp.rkt"))

(define instrument-cps (make-parameter #f))

;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal

(define run
  (lambda (string)
    (let ((cpsed-pgm
           (cps-of-program (scan&parse string))))
      (when (instrument-cps) (pretty-print cpsed-pgm))
      (value-of-program cpsed-pgm))))

(define compile
  (lambda (string)
    (cps-of-program (scan&parse string))))


;; run-all : () -> Unspecified

;; runs all the tests in test-list, comparing the results with
;; equal-answer?

(define run-all
  (lambda ()
    (run-tests! run equal-answer? test-list)))

(define equal-answer?
  (lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      (else
       (eopl:error 'sloppy->expval
                   "Can't convert sloppy value to expval: ~s"
                   sloppy-val)))))

;; run-one : Symbol -> ExpVal

;; (run-one sym) runs the test whose name is sym

(define run-one
  (lambda (test-name)
    (let ((the-test (assoc test-name test-list)))
      (cond
        ((assoc test-name test-list)
         => (lambda (test)
              (run (cadr test))))
        (else (eopl:error 'run-one "no such test: ~s" test-name))))))

;; (stop-after-first-error #t)
;; (run-all)
