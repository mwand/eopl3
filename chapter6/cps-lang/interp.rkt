#lang eopl

(require "drscheme-init.rkt")

(require "cps-out-lang.rkt")
(require "data-structures.rkt")       ; this includes environments

(provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal

(define value-of-program
  (lambda (pgm)
    (cases cps-out-program pgm
           (cps-a-program (exp1)
                          ;; (value-of/k exp1 (init-env) (end-cont))))))
                          (value-of/k exp1 (init-env) )))))

(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-expression exp
           (cps-const-exp (num) (num-val num))
           (cps-var-exp (var) (apply-env env var))

           (cps-diff-exp (exp1 exp2)
                         (let ((val1
                                (expval->num
                                 (value-of-simple-exp exp1 env)))
                               (val2
                                (expval->num
                                 (value-of-simple-exp exp2 env))))
                           (num-val
                            (- val1 val2))))

           (cps-zero?-exp (exp1)
                          (bool-val
                           (zero?
                            (expval->num
                             (value-of-simple-exp exp1 env)))))

           (cps-number?-exp (exp1)
                            (bool-val (number? (expval->num
                                                (value-of-simple-exp exp1 env)))))

           (cps-equal?-exp (exp1 exp2)
                           (let ([val1 (expval->num (value-of-simple-exp exp1 env))]
                                 [val2 (expval->num (value-of-simple-exp exp2 env))])
                             (bool-val
                              (equal? val1 val2))))

           (cps-cons-exp (head tail)
                         (let ([val1 (value-of-simple-exp head env)]
                               [val2 (value-of-simple-exp tail env)])
                           (cases expval val2
                                  (list-val (lst) (list-val (cons val1 lst)))
                                  (else
                                   (list-val (list val2 val2))))))
           
           (cps-emptylist-exp ()
                              (list-val '()))

           (cps-car-exp (lst)
                        (let ([val1 (expval->list (value-of-simple-exp lst env))])
                          (car val1)))

           (cps-cdr-exp (lst)
                        (let ([val1 (expval->list
                                     (value-of-simple-exp lst env))])
                          (list-val (cdr val1))))

           (cps-null?-exp (lst)
                          (let ([val1 (expval->list
                                       (value-of-simple-exp lst env))])
                            (bool-val (null? val1))))

           (cps-list-exp (exps)
                         (let ([vals (map
                                     (lambda (e)
                                       (value-of-simple-exp e env))
                                     exps)])
                           (list-val vals)))

           (cps-sum-exp (exps)
                        (let ((nums (map
                                     (lambda (exp)
                                       (expval->num
                                        (value-of-simple-exp exp env)))
                                     exps)))
                          (num-val
                           (let sum-loop ((nums nums))
                             (if (null? nums) 0
                                 (+ (car nums) (sum-loop (cdr nums))))))))

           (cps-proc-exp (vars body)
                         (proc-val
                          (procedure vars body env)))

           )))

;; value-of/k : TfExp * Env * Cont -> FinalAnswer
;; Page: 209
(define value-of/k
  (lambda (exp env)
    (cases tfexp exp
           (simple-exp->exp (simple)
                            (value-of-simple-exp simple env))
                            ;; (apply-cont cont
                            ;;             (value-of-simple-exp simple env)))
           (cps-let-exp (var rhs body)
                        (let ((val (value-of-simple-exp rhs env)))
                          (value-of/k body
                                      (extend-env* (list var) (list val) env)
                                      )))
           (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
                           (value-of/k letrec-body
                                       (extend-env-rec** p-names b-varss p-bodies env)
                                       ))
           (cps-if-exp (simple1 body1 body2)
                       (if (expval->bool (value-of-simple-exp simple1 env))
                           (value-of/k body1 env)
                           (value-of/k body2 env)))
           (cps-call-exp (rator rands)
                         (let ((rator-proc
                                (expval->proc
                                 (value-of-simple-exp rator env)))
                               (rand-vals
                                (map
                                 (lambda (simple)
                                   (value-of-simple-exp simple env))
                                 rands)))
                           (apply-procedure/k rator-proc rand-vals))))))

;; apply-cont : Cont * ExpVal -> Final-ExpVal
;; there's only one continuation, and it only gets invoked once, at
;; the end of the computation.
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont () val))))

;; apply-procedure/k : Proc * ExpVal * Cont -> ExpVal
;; Page: 209
(define apply-procedure/k
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (value-of/k body
                                  (extend-env* vars args saved-env)
                                  )))))

'(define apply-procedure/k
   (lambda (proc1 args)
     (cases proc proc1
            (procedure (vars body saved-env)
                       (value-of/k body
                                   (extend-env* vars args saved-env)
                                   )))))

;; trace has to be in the module where the procedure is defined.
;; (trace value-of/k apply-cont)
