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


;; list-sum : Listof(Int) -> Int
(define list-sum
   (lambda (lst)
     (list-sum/k lst (end-cont))))

(define list-sum/k
  (lambda (lst cont)
    (if (null? lst)
        (apply-cont cont 0)
        (list-sum/k (cdr lst)
                    (list-sum1-cont (car lst) cont)))))

(define list-sum1-cont
  (lambda (head cont)
    (lambda (val)
      (apply-cont cont (+ head val)))))

;; occurs-free?
(define occurs-free?
  (lambda (var exp)
    (occurs-free/k var exp (end-cont))))

(define occurs-free/k
  (lambda (var exp cont)
    (cond
      [(symbol? exp) (apply-cont cont (eqv? var exp))]
      [(eqv? (car exp) 'lambda)
       (occurs-free/k var (caddr exp)
                      (occurs-free1-cont
                       (not (eqv? var (car (cadr exp))))
                       cont))]
      [else
       (occurs-free/k var (car exp)
                      (occurs-free2-cont var (cadr exp) cont))]
      )))

(define occurs-free1-cont
  (lambda (b cont)
    (lambda (val)
      (apply-cont cont (and b val)))))

(define occurs-free2-cont
  (lambda (var exp cont)
    (lambda (val)
    (occurs-free/k var exp (occurs-free3-cont val cont)))))

(define occurs-free3-cont
  (lambda (b cont)
    (lambda (val)
      (apply-cont cont (or b val)))))

(module+ test
  (check-equal? (remove-fst 'a '(a b c)) '(b c))
  (check-equal? (remove-fst 'b '(e f g)) '(e f g))
  (check-equal? (remove-fst 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
  (check-equal? (remove-fst 'x '()) '())
  (check-equal? (list-sum '(1 2 3 4 5)) 15)
  (check-equal? (list-sum '(1 2 3 4)) 10)
  (check-equal? (list-sum '()) 0)
  (check-equal? (occurs-free? 'x 'x) #t)
  (check-equal? (occurs-free? 'x 'y) #f)
  (check-equal? (occurs-free? 'x '(lambda (x) (x y))) #f)
  (check-equal? (occurs-free? 'x '(lambda (y) (x y))) #t)
  (check-equal? (occurs-free? 'x '((lambda (x) x) (x y))) #t)
  (check-equal? (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))) #t)
  )
