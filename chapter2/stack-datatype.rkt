#lang racket

(require rackunit)
(require eopl)

(define-datatype stack stack?
  (empty-stack)
  (push
   (elem symbol?)
   (s stack?)))

(define top
  (lambda (s)
    (cases stack s
           (empty-stack () (eopl:error "It's a empty stack"))
           (push (elem s) elem))))

(define pop
  (lambda (s)
    (cases stack s
           (empty-stack () (eopl:error "It's a empty stack"))
           (push (elem s) s))))

(define s
  (push 'a (push 'b (push 'c (empty-stack)))))

(module+ test
  (check-equal? (top s) 'a)
  (check-equal? (top (pop s)) 'b)
  (check-equal? (top (pop (pop s))) 'c)
  )
