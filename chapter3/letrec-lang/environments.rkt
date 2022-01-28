#lang eopl
;; builds environment interface, using data structures defined in
;; data-structures.scm.

(require "data-structures.rkt")

(provide init-env empty-env extend-env extend-env* extend-env-rec apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; usage: (init-env) = [i=1, v=5, x=10]
;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.
;; Page: 69

;; (define init-env
;;   (lambda ()
;;     (extend-env
;;      'i (num-val 1)
;;      (extend-env
;;       'v (num-val 5)
;;       (extend-env
;;        'x (num-val 10)
;;        (empty-env))))))
(define init-env empty-env)

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;; Page: 86
(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (eopl:error 'apply-env "No binding for ~s" search-sym))
           (extend-env (var val saved-env)
                       (if (eqv? search-sym var)
                           (if (expval? val)
                               val
                               (vector-ref val 0))
                           (apply-env saved-env search-sym))))))


(define extend-env-rec
  (lambda (p-name b-var body saved-env)
    (let* ([vec (make-vector 1)]
           [new-env (extend-env p-name vec saved-env)])
      (vector-set! vec 0
                   (proc-val (procedure b-var body new-env)))
      new-env)))

(define (extend-env* vars vals saved-env)
  (if (or (null? vars) (null? vals))
      saved-env
      (extend-env* (cdr vars) (cdr vals)
                   (extend-env (car vars) (car vals) saved-env))))
