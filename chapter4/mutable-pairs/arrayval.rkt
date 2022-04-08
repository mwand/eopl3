#lang eopl

(require (only-in racket make-list))

(require "drscheme-init.rkt")
(require "store.rkt")

(provide (all-defined-out))

;; (define array?
;;   (lambda (v)
;;     (reference? v)))
(define-datatype array array?
  (a-array
   (start reference?)
   (len integer?)))

(define newarray
  (lambda (size initial)
    (let ([refs
           (map newref (make-list size initial))])
      (if (null? refs)
          (eopl:error 'newarray "array size must bigger than 0")
          (a-array (car refs)
                   size)))))

(define arrayref
  (lambda (arr idx)
    (cases array arr
           (a-array (start len)
                    (if (> idx len)
                        (eopl:error 'arrayref "out of array index")
                        (deref (+ idx start)))))))
    ;; (deref (+ idx arr))))

(define arrayset!
  (lambda (arr idx val)
    (cases array arr
           (a-array (start len)
                    (if (> idx len)
                        (eopl:error 'arrayset! "out of array index")
                        (setref! (+ idx start) val))))))
    ;; (setref! (+ idx arr) val)))

