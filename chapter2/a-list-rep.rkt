#lang eopl

(require "utils.rkt")

(define empty-env (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

(define apply-env
  (lambda (env search-var)
    (cond
      [(null? (car env))
       (report-no-binding-found search-var)]
      [(eqv? (caar env) search-var) (cdar env)]
      [else
       (apply-env (cdr env) search-var)])))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))

(equal?? (apply-env e 'd) 6)
(equal?? (apply-env e 'y) 8)
(equal?? (apply-env e 'x) 7)

(report-unit-tests-completed 'apply-env)
