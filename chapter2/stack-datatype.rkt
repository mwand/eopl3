#lang eopl

(require "utils.rkt")

(define empty-stack? null?)

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

(equal? (top s) 'a)
(equal?? (top (pop s)) 'b)

(report-unit-tests-completed 'stack-datatype)
