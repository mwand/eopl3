#lang eopl

;; Simple procedural representation of environments
;; Page: 40

(require "utils.rkt")

;; data definition:
;; Env = Var -> Schemeval

;; empty-env : () -> Env
(define empty-env
  (lambda ()
    (list
     (lambda (search-var)
       (report-no-binding-found search-var))
     (lambda () #t))))

;; extend-env : Var * Schemeval * Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list 
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))
     (lambda () #f))))

;; apply-env : Env * Var -> Schemeval
(define apply-env
  (lambda (env search-var) 
    ((car env)  search-var)))


;; ex 2.13, ex 2.14
(define empty-env?
  (lambda (env)
    ((cadr env))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))

(equal?? (apply-env e 'd) 6)
(equal?? (apply-env e 'y) 8)
(equal?? (apply-env e 'x) 7)
(equal?? (empty-env? (empty-env)) #t)

(report-unit-tests-completed 'apply-env)
