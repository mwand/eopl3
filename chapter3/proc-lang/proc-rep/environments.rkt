#lang eopl

;; builds environment interface, using data structures defined in
;; data-structures.scm. 

(require "data-structures.rkt")

(provide init-env empty-env extend-env extend-env* apply-env bind-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; usage: (init-env) = [i=1, v=5, x=10]
;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.
;; Page: 69
(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define empty-env
  (lambda ()
    (empty-env-record)))

(define empty-env? 
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define (extend-env* vars vals env)
  (if (or (null? vars) (null? vals))
      env
      (extend-env* (cdr vars) (cdr vals)
                   (extend-env (car vars) (car vals) env))))

;; (define (extend-envrec vars exps env)
;;   (if (or (null? vars) (null? exps))
;;       env
;;       (extend-envrec (cdr vars) (cdr exps)
;;                      (extend-env (car vars) (value-of (car exps) env) env))))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ((sym (extended-env-record->sym env))
              (val (extended-env-record->val env))
              (old-env (extended-env-record->old-env env)))
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))

(define bind-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (list #f)
        (let ((sym (extended-env-record->sym env))
              (val (extended-env-record->val env))
              (old-env (extended-env-record->old-env env)))
          (if (eqv? search-sym sym)
               (list #t val)
              (bind-env old-env search-sym))))))

