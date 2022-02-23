#lang eopl

(require "utils.rkt")

(define empty-stack (lambda () '()))

(define empty-stack? (lambda (stack) (null? stack)))

(define push
  (lambda (stack val)
    (cons val stack)))

(define pop
  (lambda (stack)
    (cond
      [(empty-stack? stack)
       (eopl:error "This is empty stack: ~s" stack)]
      [else
       (cdr stack)])))

(define top
  (lambda (stack)
    (cond
      [(empty-stack? stack)
       (eopl:error "This is empty stack: ~s" stack)]
      [else
       (car stack)])))

(define s
  (push (push (push (empty-stack) 'a)
              'b)
        'c))

(top s)
(equal?? (top s) 'c)
(equal?? (pop s) '(b a))

(report-unit-tests-completed 'stack-rep)
