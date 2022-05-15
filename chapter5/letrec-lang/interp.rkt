#lang eopl

;; cps interpreter for the LETREC language, using the data structure
;; representation of continuations (Figure 5.3).

;; exercise: rewrite this using the procedural representation of
;; continuations (Figure 5.2).

;; exercise: rewrite this using a trampoline (page 159).

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> FinalAnswer
;; Page: 143 and 154
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of/k exp1 (init-env) (end-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 143--146, and 154
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           (const-exp (num) (apply-cont cont (num-val num)))
           (var-exp (var) (apply-cont cont (apply-env env var)))
           (proc-exp (var body)
                     (apply-cont cont
                                 (proc-val (procedure var body env))))
           (letrec-exp (p-name b-var p-body letrec-body)
                       (value-of/k letrec-body
                                   (extend-env-rec p-name b-var p-body env)
                                   cont))
           (zero?-exp (exp1)
                      (value-of/k exp1 env
                                  (zero1-cont cont)))
           (let-exp (vars exps body)
                    (if (null? vars)
                        (value-of/k body env cont)
                        (value-of/k (car exps) env
                                    (let-head-cont vars (cdr exps) body env cont))))
                    ;; (value-of/k exp1 env
                    ;;             (let-exp-cont var body env cont)))
           (let2-exp (var1 exp1 var2 exp2 body)
                     (value-of/k exp1 env
                                 (let2-exp-cont var1 var2 exp2 body env cont)))
           (let3-exp (var1 exp1 var2 exp2 var3 exp3 body)
                     (value-of/k exp1 env
                                 (let3-exp-cont var1 var2 exp2 var3 exp3 body env cont)))
           (if-exp (exp1 exp2 exp3)
                   (value-of/k exp1 env
                               (if-test-cont exp2 exp3 env cont)))
           (diff-exp (exp1 exp2)
                     (value-of/k exp1 env
                                 (diff1-cont exp2 env cont)))
           (call-exp (rator rand)
                     (value-of/k rator env
                                 (rator-cont rand env cont)))
           (emptylist-exp ()
                          (apply-cont cont (list-val '())))
           (cons-exp (head tail)
                     (value-of/k head env
                                 (cons-cont tail env cont)))
           (car-exp (lst)
                     (car
                      (expval->list
                       (value-of/k lst env cont))))
           (cdr-exp (lst)
                    (list-val
                     (cdr
                      (expval->list
                       (value-of/k lst env cont)))))
           (null?-exp (lst)
                      (bool-val
                       (null?
                        (expval->list
                         (value-of/k lst env cont)))))

           (list-exp (lst)
                     (if (null? lst)
                         (apply-cont cont (list-val '()))
                         (value-of/k (car lst) env
                                     (lst-head-cont (cdr lst) env cont))))
           )))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; Page: 148
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont ()
                     (begin
                       (eopl:printf
                        "End of computation.~%")
                       val))
           ;; or (logged-print val)  ; if you use drscheme-init-cps.rkt
           (zero1-cont (saved-cont)
                       (apply-cont saved-cont
                                   (bool-val
                                    (zero? (expval->num val)))))
           (let-exp-cont (var body saved-env saved-cont)
                         (value-of/k body
                                     (extend-env var val saved-env) saved-cont))
           (let-head-cont (vars exps body saved-env saved-cont)
                          (if (null? exps)
                              (value-of/k body
                                          (extend-env (car vars) val saved-env)
                                          saved-cont)
                              (value-of/k (let-exp (cdr vars) exps body)
                                          (extend-env (car vars) val saved-env)
                                          saved-cont)))
           (let2-exp-cont (var1 var2 exp2 body saved-env saved-cont)
                          (let ([new-env (extend-env var1 val saved-env)]
                                [new-exp [let-exp var2 exp2 body]])
                            (value-of/k new-exp new-env saved-cont)))
           (let3-exp-cont (var1 var2 exp2 var3 exp3 body saved-env saved-cont)
                          (let ([new-env (extend-env var1 val saved-env)]
                                [new-exp (let2-exp var2 exp2 var3 exp3 body)])
                            (value-of/k new-exp new-env saved-cont)))
           (if-test-cont (exp2 exp3 saved-env saved-cont)
                         (if (expval->bool val)
                             (value-of/k exp2 saved-env saved-cont)
                             (value-of/k exp3 saved-env saved-cont)))
           (diff1-cont (exp2 saved-env saved-cont)
                       (value-of/k exp2
                                   saved-env (diff2-cont val saved-cont)))
           (diff2-cont (val1 saved-cont)
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val)))
                         (apply-cont saved-cont
                                     (num-val (- num1 num2)))))
           (rator-cont (rand saved-env saved-cont)
                       (value-of/k rand saved-env
                                   (rand-cont val saved-cont)))
           (rand-cont (val1 saved-cont)
                      (let ((proc (expval->proc val1)))
                        (apply-procedure/k proc val saved-cont)))
           (cons-cont (tail saved-env saved-cont)
                      (if (equal? tail (emptylist-exp))
                          (apply-cont saved-cont
                                      (list-val (list val)))
                          (list-val
                           (cons val
                                 (expval->list
                                  (value-of/k tail saved-env saved-cont))))))
           (lst-head-cont (tail saved-env saved-cont)
                          (if (null? tail)
                              (apply-cont saved-cont
                                          (list-val
                                           (cons val '())))
                              (value-of/k (list-exp tail)
                                          saved-env
                                          (lst-tail-cont val saved-cont))))
           (lst-tail-cont (val1 saved-cont)
                          (apply-cont saved-cont
                                      (list-val
                                       (cons val1 (expval->list val)))))
           )))

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
;; Page 152 and 155
(define apply-procedure/k
  (lambda (proc1 arg cont)
    (cases proc proc1
           (procedure (var body saved-env)
                      (value-of/k body
                                  (extend-env var arg saved-env)
                                  cont)))))

;; (define end-cont
;;   (lambda ()
;;     (lambda (val)
;;       (begin (eopl:printf "End of computation. ~%")
;;              val))))

;; (define zero1-cont
;;   (lambda (saved-cont)
;;     (lambda (val)
;;       (apply-cont saved-cont
;;                   (bool-val
;;                    (zero? (expval->num val)))))))

;; (define let-exp-cont
;;   (lambda (var body saved-env saved-cont)
;;     (lambda (val)
;;       (value-of/k body
;;                   (extend-env var val saved-env) saved-cont))))

;; (define if-test-cont
;;   (lambda (exp2 exp3 saved-env saved-cont)
;;     (lambda (val)
;;       (if (expval->bool val)
;;           (value-of/k exp2 saved-env saved-cont)
;;           (value-of/k exp3 saved-env saved-cont)))))

;; (define diff1-cont
;;   (lambda (exp2 saved-env saved-cont)
;;     (lambda (val)
;;       (value-of/k exp2
;;                   saved-env (diff2-cont val saved-cont)))))

;; (define diff2-cont
;;   (lambda (val1 saved-cont)
;;     (lambda (val)
;;       (apply-cont saved-cont
;;                   (num-val (- (expval->num val1)
;;                               (expval->num val)))))))

;; (define rator-cont
;;   (lambda (rand saved-env saved-cont)
;;     (lambda (val)
;;       (value-of/k rand saved-env
;;                   (rand-cont val saved-cont)))))

;; (define rand-cont
;;   (lambda (val1 saved-cont)
;;     (lambda (val)
;;       (apply-procedure/k (expval->proc val1)
;;                          val saved-cont))))

;; (define apply-cont
;;   (lambda (cont val)
;;     (cont val)))
