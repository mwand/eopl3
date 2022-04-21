#lang eopl

;; require both recursive and register versions.
;; test with  (interp-run-all) or (registers-run-all)
;; (run-all) will run both.

(require (prefix-in interp- "top-interp.rkt"))
(require (prefix-in registers- "top-interp-registers.rkt"))

(provide interp-run registers-run run-all)

(define run-all
  (lambda ()
    (interp-run-all)
    (registers-run-all)))

;;; interface for book test ;;;
(provide test-all)
(define (test-all)
  (run-all))


