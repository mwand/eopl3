#lang racket

;; remove "exp" from the eopl language level, because we use it as
;; a mutable variable.

(require (lib "eopl.ss" "eopl"))
(provide (except-out (all-from-out (lib "eopl.ss" "eopl")) exp))

