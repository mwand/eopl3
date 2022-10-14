#lang eopl

(require rackunit)
(require racket)

(define end-cont
  (lambda ()
    (lambda (val)
      (begin (eopl:printf "End of computation.~%")
             (eopl:printf "This sentence should appear only once.~%")
             val))))

(define remove-fst1-cont
  (lambda (head cont)
    (lambda (val)
      (apply-cont cont (cons head val)))))


(define apply-cont
  (lambda (cont val)
    (cont val)))

(define remove-fst/k
  (lambda (sym lst cont)
    (if (null? lst)
        (apply-cont cont '())
        (if (eqv? (car lst) sym)
            (apply-cont cont (cdr lst))
            (remove-fst/k sym (cdr lst)
                          (remove-fst1-cont (car lst) cont))))))

(define remove-fst
  (lambda (sym lst)
    (remove-fst/k sym lst (end-cont))))

(module+ test
  (check-equal? (remove-fst 'a '(a b c)) '(b c))
  (check-equal? (remove-fst 'b '(e f g)) '(e f g))
  (check-equal? (remove-fst 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
  (check-equal? (remove-fst 'x '()) '())
  )
