#lang eopl

;; interpreter for the LETREC language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 83
(define value-of
  (lambda (exp env)
    (cases expression exp

           ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
           (const-exp (num) (num-val num))

           ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
           (var-exp (var) (apply-env env var))

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
           (let-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var val1 env))))

           (proc-exp (vars body)
                     (proc-val (procedure vars body env)))

           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (args (map
                                  (lambda (e) (value-of e env))
                                  rand)))
                       (begin (display env)
                              (newline)
                              (display proc)
                              (newline)
                              (display "----------")
                              (newline))
                       (apply-procedure proc args)))

           (letrec-exp (p-name b-var p-body letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec p-name b-var p-body env)))
           ;; (letrec-exp (p-names b-vars p-bodys letrec-body)
           ;;             (let [(new-env (extend-env-rec p-names b-vars p-bodys env))]
           ;;               (begin
           ;;                 (display new-env)
           ;;                 (newline)
           ;;                 (display "++++++++++++++")
           ;;                 (newline)
           ;;               (value-of letrec-body new-env))))
                                   ;; (extend-env-rec* p-names b-vars p-bodys env)))

           )))

;; apply-procedure : Proc * ExpVal -> ExpVal

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (value-of body (extend-env* vars args saved-env))))))

;; helper for extend-env-rec list
;; (define (extend-env-rec* p-names b-vars p-bodys env)
;;   (if (or (null? p-names) (null? b-vars) (null? p-bodys))
;;       env
;;       ;; (begin (display env)
;;       ;;        (newline)
;;       ;;        (display "===========")
;;       ;;        (newline)
;;       (extend-env-rec* (cdr p-names)
;;                        (cdr b-vars)
;;                        (cdr p-bodys)
;;                        (extend-env-rec (car p-names)
;;                                        (car b-vars)
;;                                        (car p-bodys)
;;                                        ;; (value-of (car p-bodys) env)
;;                                        env))))
