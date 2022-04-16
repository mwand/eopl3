#lang eopl

;; interpreter for the IMPLICIT-REFS language
(require (only-in racket
                  filter))

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")
(require "store.rkt")

(provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

(define instrument-let (make-parameter #f))

;; say (instrument-let #t) to turn instrumentation on.
;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 118, 119
(define value-of
  (lambda (exp env)
    (cases expression exp

           ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
           (const-exp (num) (num-val num))

           ;\commentbox{ (value-of (var-exp \x{}) \r)
           ;              = (deref (apply-env \r \x{}))}
           (var-exp (var) (deref (apply-env env var)))

           ;\commentbox{\diffspec}
           (diff-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (num-val
                          (- num1 num2)))))

           ;\commentbox{\zerotestspec}
           (zero?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1)))
                          (if (zero? num1)
                              (bool-val #t)
                              (bool-val #f)))))

           ;\commentbox{\ma{\theifspec}}
           (if-exp (exp1 exp2 exp3)
                   (let ((val1 (value-of exp1 env)))
                     (if (expval->bool val1)
                         (value-of exp2 env)
                         (value-of exp3 env))))

           ;\commentbox{\ma{\theletspecsplit}}
           (let-exp (vars exps body)
                    (let ([vals (map
                                 (lambda (e) (value-of e env))
                                 exps)])
                      (value-of body
                                (extend-env* vars (map newref vals) env))))

           (proc-exp (vars body)
                     (proc-val (procedure vars body env)))

           (call-exp (rator rands)
                     (let ([proc (expval->proc (value-of rator env))]
                           [args ;;(value-of rand env)))
                            (map (lambda (e) (value-of e env)) rands)])
                       (apply-procedure proc args)))

           (letrec-exp (p-names b-vars p-bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec* p-names b-vars p-bodies env)))

           (begin-exp (exp1 exps)
                      (letrec
                          ((value-of-begins
                            (lambda (e1 es)
                              (let ((v1 (value-of e1 env)))
                                (if (null? es)
                                    v1
                                    (value-of-begins (car es) (cdr es)))))))
                        (value-of-begins exp1 exps)))

           (assign-exp (var exp1)
                       (begin
                         (setref!
                          (apply-env env var)
                          (value-of exp1 env))
                         (num-val 27)))

           ;; vars must be bound vars
           (setdynamic-exp (vars exps body)
                           (let* ([refs
                                   (map (lambda (v)
                                          (apply-env env v))
                                        vars)]
                                  [old-vals
                                   (map deref refs)]
                                  [new-vals
                                   (map (lambda (v) (value-of v env))
                                        exps)]
                                  [vbody (begin
                                           (map
                                            (lambda (r v)
                                              (setref! r v))
                                            refs new-vals)
                                           (value-of body env))])
                             (begin
                               (map (lambda (r v)
                                      (setref! r v))
                                    refs old-vals)
                               vbody)))

           ;; new for ref-exp
           (ref-exp (var)
                    (apply-env env var))

           (deref-exp (var)
                      (deref
                       (value-of (var-exp var) env)))
                      ;; (deref var))

           (setref-exp (var exp1)
                       (begin
                         (setref! (value-of (var-exp var) env)
                                  (value-of exp1 env))
                         ;; (setref! (apply-env env var) (value-of exp1 env))
                         ;; (setref! var
                         ;;          (value-of exp1 env))
                         (num-val 28)))
           )))


;; apply-procedure : Proc * ExpVal -> ExpVal
;; Page: 119

;; uninstrumented version
;;  (define apply-procedure
;;    (lambda (proc1 val)
;;      (cases proc proc1
;;        (procedure (var body saved-env)
;;          (value-of body
;;            (extend-env var (newref val) saved-env))))))

;; instrumented version
(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (let* ([r (map newref args)]
                             [new-env (extend-env* vars r saved-env)])
                        ;; (let ((new-env (extend-env var r saved-env)))
                        (when (instrument-let)
                          (begin
                            (eopl:printf
                             "entering body of proc ~s with env =~%"
                             vars)
                            (pretty-print (env->list new-env))
                            (eopl:printf "store =~%")
                            (pretty-print (store->readable (get-store-as-list)))
                            (eopl:printf "~%")))
                        (value-of body new-env))))))

;; store->readable : Listof(List(Ref,Expval))
;;                    -> Listof(List(Ref,Something-Readable))
(define store->readable
  (lambda (l)
    (map
     (lambda (p)
       (list
        (car p)
        (expval->printable (cadr p))))
     l)))





