#lang racket

(require rackunit)

(define empty-stack
  (lambda ()
    (lambda (op)
      (cond
        [(eqv? op 'empty) #t]
        [else
         (error "This is a empty stack")]))))

(define empty-stack? (lambda (s) (s 'empty)))

(define push
  (lambda (val stack)
    (lambda (op)
      (cond
        [(eqv? op 'top) val]
        [(eqv? op 'pop) stack]
        [(eqv? op 'empty) #f]
        [else
         (error "Not a proper operator")]))))

(define pop (lambda (s) (s 'pop)))

(define top (lambda (s) (s 'top)))

(define s
  (push 'a (push 'b (push 'c (empty-stack)))))


(module+ test
  (check-equal? (top s) 'c)
  (check-equal? (top (pop s)) 'b)
  (check-equal? (empty-stack? (empty-stack)) #t)
  (check-equal? (empty-stack? s) #f)
  )
