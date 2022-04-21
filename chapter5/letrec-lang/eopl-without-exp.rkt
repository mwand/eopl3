#lang racket

;; remove "exp" from the eopl language level, because we use it as
;; a mutable variable.

(require eopl)
(provide (except-out (all-from-out eopl) exp))

