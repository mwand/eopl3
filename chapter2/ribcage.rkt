#lang eopl

(require "utils.rkt")

(define empty-env (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (cons (list var) (list (list val))) env)))

(define extend-env*
  (lambda (vars vals env)
    (cons (cons vars (list vals)) env)))

(define apply-env
  (lambda (env search-var)
    (cond
      [(null? (car env))
       (report-no-binding-found search-var)]
      [(eqv? (caaar env) search-var) (caadar env)]
      [(null? (cdaar env))
       (apply-env (cdr env) search-var)]
      [else
       (apply-env
        (cons (cons (cdaar env) (list (cdadar env))) (cdr env))
        search-var)])))

(define empty-env?
  (lambda (env)
    (null? env)))

;; (define has-binding?
;;   (lambda (s env)
;;     (cond
;;       [(null? env) #f]
;;       [(eqv? (caar env) s) #t]
;;       [else
;;        (has-binding? s (cdr env))])))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))
(define e1
  (extend-env* '(a b c) '(1 2 3 4) (empty-env)))

;; (equal?? (apply-env e 'd) 6)
;; (equal?? (apply-env e 'y) 8)
;; (equal?? (apply-env e 'x) 7)
;(equal?? (has-binding? 'z e) #f)

;(equal?? (apply-env e1 'a) 1)

(report-unit-tests-completed 'apply-env)
