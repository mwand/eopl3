#lang eopl

(require racket)
(require rackunit)
(require racket/trace)

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

(define remove-fst
  (lambda (sym lst)
    (remove-fst/k sym lst (end-cont))))

(define remove-fst/k
  (lambda (sym lst cont)
    (if (null? lst)
        (apply-cont cont '())
        (if (eqv? (car lst) sym)
            (apply-cont cont (cdr lst))
            (remove-fst/k sym (cdr lst)
                          (remove-fst1-cont (car lst) cont))))))

(define-datatype continuation continuation?
  (end-cont)
  (remove-fst1-cont
   (head symbol?)
   (saved-cont continuation?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont ()
                     (begin
                       (eopl:printf "End of computation.~%")
                       (eopl:printf "This sentence should appear only once.~%")
                       val))
           (remove-fst1-cont (head cont)
                             (apply-cont cont (cons head val))))))

(module+ test
  (check-equal? (remove-first 'a '(a b c)) '(b c))
  (check-equal? (remove-first 'b '(e f g)) '(e f g))
  (check-equal? (remove-first 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
  (check-equal? (remove-first 'x '()) '())
  (check-equal? (remove-fst 'a '(a b c)) '(b c))
  (check-equal? (remove-fst 'b '(e f g)) '(e f g))
  (check-equal? (remove-fst 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
  (check-equal? (remove-fst 'x '()) '())
  )

;; (trace remove-fst remove-fst/k apply-cont)
