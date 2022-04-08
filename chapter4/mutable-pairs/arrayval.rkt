#lang eopl

(require (only-in racket make-list))

(require "drscheme-init.rkt")
(require "store.rkt")

(provide (all-defined-out))

(define array?
  (lambda (v)
    (reference? v)))

(define newarray
  (lambda (size initial)
    (let ([refs
           (map newref (make-list size initial))])
      (if (null? refs)
          (eopl:error 'newarray "array size must bigger than 0")
          (car refs)))))

(define arrayref
  (lambda (arr idx)
    (deref (+ idx arr))))

(define arrayset!
  (lambda (arr idx val)
    (setref! (+ idx arr) val)))

