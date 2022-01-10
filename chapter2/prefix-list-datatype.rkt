#lang racket

(require eopl)
(require rackunit)

;; Prefix-list ::=(Prefix-exp)
;; Prefix-exp ::= Int
;;            ::=- Prefix-exp Prefix-exp

;; Exercise 2.31
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))


(define (parse-prefix-list-rec l exp)
    (cond [(null? l) (cons '() exp)]
          [(and (equal? (car l) '-) (not (null? exp)))
           (parse-prefix-list-rec
            (cdr l)
            (cons (diff-exp (car exp) (cadr exp))
                  (cddr exp)))]
          [(number? (car l))
           (parse-prefix-list-rec
            (cdr l)
            (cons (const-exp (car l)) exp))]
        ))

;; prefix-list-parser :: List -> Prefix-exp
(define parse-prefix-list
  (lambda (a-list)
    (let ([rlist (reverse a-list)])
      (cadr
       (parse-prefix-list-rec rlist '())))))

(define e1
  (diff-exp
   (diff-exp
    (const-exp 3)
    (const-exp 2))
   (diff-exp
    (const-exp 4)
    (diff-exp
     (const-exp 12)
     (const-exp 7))))
  )

(module+ test
  (check-equal? (parse-prefix-list '(- - 3 2 - 4 - 12 7))
                e1))
