#lang eopl

;; Simple procedural representation of environments
;; Page: 40

(require "utils.rkt")

(install-eopl-exception-handler)

;; data definition:
;; Env = Var -> Schemeval

;; empty-env : () -> Env
(define empty-env
  (lambda ()
    (list
     (lambda (search-var)
       (report-no-binding-found search-var))
     (lambda (search-var)
       #f)
     (lambda () #t))))

;; extend-env : Var * Schemeval * Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           #t
           (has-binding? saved-env search-var)))
     (lambda () #f))))

;; apply-env : Env * Var -> Schemeval
(define apply-env
  (lambda (env search-var)
    ((car env)  search-var)))


;; ex 2.13, ex 2.14
(define empty-env?
  (lambda (env)
    ((caddr env))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define has-binding?
  (lambda (env s)
    ((cadr env) s)))

;; (define report-invalid-env
;;   (lambda (env)
;;     (eopl:error 'apply-env "Bad environment: ~s" env)))

(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))


;; howto evaulate e??
;; (e 'd)
(equal?? (apply-env e 'd) 6)
(equal?? (apply-env e 'y) 8)
(equal?? (apply-env e 'x) 7)
(equal?? (empty-env? (empty-env)) #t)
(equal?? (empty-env? e) #f)
(equal?? (has-binding? e 'd) #t)
(equal?? (has-binding? e 'a) #f)

(report-unit-tests-completed 'apply-env)
