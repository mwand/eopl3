#lang eopl

(require "utils.rkt")

(define empty-stack
  (lambda ()
    (lambda (op)
      (cond
        [(eqv? op 'empty) #t]
        [else
         (eopl:error "This is a empty stack")]))))

(define empty-stack? (lambda (s) (s 'empty)))

(define push
  (lambda (val stack)
    (lambda (op)
      (cond
        [(eqv? op 'top) val]
        [(eqv? op 'pop) stack]
        [(eqv? op 'empty) #f]
        [else
         (eopl:error "Not a proper operator")]))))

(define pop (lambda (s) (s 'pop)))

(define top (lambda (s) (s 'top)))

(define s
  (push 'a (push 'b (push 'c (empty-stack)))))

(equal?? (top s) 'a)
(equal?? (top (pop s)) 'b)
(equal?? (empty-stack? (empty-stack)) #t)
(equal?? (empty-stack? s) #f)

(report-unit-tests-completed 'stack-rep)
