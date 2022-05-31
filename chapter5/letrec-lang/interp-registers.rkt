;; (module interp-registers "eopl-without-exp.rkt"
#lang racket

;; require EOPL except exp
(require (except-in eopl exp))

;; imperative cps interpreter for the LETREC language, using the
;; data structure representation of continuations (Figure 5.3)

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")
(require "store.rkt")

(provide value-of-program value-of/k instrument-newref)

(provide trace-apply-procedure trace-value-of/k trace-trampoline)

(define trace-apply-procedure (make-parameter #f))
(define trace-value-of/k (make-parameter #f))
(define trace-trampoline (make-parameter #f))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;;; have the interpreter procedures communicate via registers

(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)         ; we've already used "proc".
(define pc 'uninitialized)

;; value-of-program : Program -> FinalAnswer
;; Page: 167
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
           (a-program (body)
                      (set! cont (end-cont))
                      (set! exp body)
                      (set! env (init-env))
                      (set! pc value-of/k)
                      (trampoline)))))
;; (set! pc #f)
;; (trampoline)
;; (value-of/k)))))

(define trampoline
  (lambda ()
    (when (trace-trampoline)
      (eopl:printf "exp=~s~%val=~s~%env=~s~%cont=~s~%pc=~s~%~%"
                   exp val env cont pc))
    (if pc
        (begin
          (pc)
          (trampoline))
        val)))

;; value-of : Exp * Env * Cont -> FinalAnswer
;; value-of/k : () -> FinalAnswer
;; usage : relies on registers
;;      exp  : Exp
;;      env  : Env
;;      cont : Cont
;; Page 167 and 168
;;
;; The code from the corresponding portions of interp.rkt is shown
;; as comments.
(define value-of/k
  (lambda ()
    (when (trace-value-of/k)
      (eopl:printf "exp=~s~%val=~s~%env=~s~%cont=~s~%pc=~s~%~%"
                   exp val env cont pc))
    (cases expression exp
           (const-exp (num)
                      ;; (apply-cont cont (num-val num)))
                      (set! val (num-val num))
                      ;; cont is unchanged
                      (apply-cont))
           (var-exp (var)
                    ;; (apply-cont cont (apply-env env id)))
                    (set! val (deref (apply-env env var)))
                    ;; cont is unchanged
                    (apply-cont))
           (proc-exp (var body)
                     ;; (apply-cont cont (proc-val (procedure bvar body env))
                     (set! val (proc-val (procedure var body env)))
                     (apply-cont))
           (letrec-exp (p-name b-var p-body letrec-body)
                       ;; (value-of/k letrec-body
                       ;;   (extend-env-rec proc-name bvar proc-body env)
                       ;;   cont)
                       (set! exp letrec-body)
                       (set! env
                             (extend-env-rec* p-name b-var p-body env))
                       (value-of/k))
           (zero?-exp (exp1)
                      ;; (value-of/k exp1 env (zero1-cont cont))
                      (set! cont (zero1-cont cont))
                      (set! exp exp1)
                      (value-of/k))
           (let-exp (vars exps body)
                    ;; (value-of/k rhs env (let-exp-cont id body env cont))
                    (if (null? exps)
                        (value-of/k)
                        (begin
                          (set! cont (let-head-cont vars (cdr exps) body env cont))
                          (set! exp (car exps))
                          (value-of/k))))
           (let2-exp (var1 exp1 var2 exp2 body)
                     (set! exp exp1)
                     (set! cont (let2-exp-cont var1 var2 exp2 body env cont))
                     (value-of/k))
           (let3-exp (var1 exp1 var2 exp2 var3 exp3 body)
                     (set! exp exp1)
                     (set! cont (let3-exp-cont var1 var2 exp2 var3 exp3 body env cont))
                     (value-of/k))
           (if-exp (exp1 exp2 exp3)
                   ;; (value-of/k exp0 env (if-test-cont exp2 exp3 env cont))
                   (set! cont (if-test-cont exp2 exp3 env cont))
                   (set! exp exp1)
                   (value-of/k))
           (diff-exp (exp1 exp2)
                     ;; (value-of/k exp1 env (diff1-cont exp2 env cont))
                     (set! cont (diff1-cont exp2 env cont))
                     (set! exp exp1)
                     ;; env is unchanged
                     (value-of/k))
           (multi-exp (exp1 exp2)
                      (set! exp exp1)
                      (set! cont (multi1-cont exp2 env cont))
                      (value-of/k))
           (call-exp (rator rands)
                     ;; (value-of/k rator env (rator-cont rand env cont))
                     (set! cont (rator-cont rands env cont))
                     (set! exp rator)
                     (value-of/k))
           (emptylist-exp ()
                          (set! val (list-val '()))
                          (apply-cont))
           (cons-exp (head tail)
                     (set! exp head)
                     (set! cont (cons-cont tail env cont))
                     (value-of/k))
           (car-exp (lst)
                    (set! exp lst)
                    (set! cont (car-cont cont))
                    (value-of/k))
           (cdr-exp (lst)
                    (set! exp lst)
                    (set! cont (cdr-cont cont))
                    (value-of/k))
           (null?-exp (lst)
                      (set! exp lst)
                      (set! cont (null?-cont cont))
                      (value-of/k))
           (list-exp (lst)
                     (if (null? lst)
                         (begin (set! val (list-val '()))
                                (apply-cont))
                         (begin (set! exp (car lst))
                                (set! cont (lst-head-cont (cdr lst) env cont))
                                (value-of/k))))
           (assign-exp (var exp1)
                       (let ([ref (apply-env env var)])
                         (set! exp exp1)
                         (set! cont (set-rhs-cont ref cont))
                         (value-of/k)))
           (begin-exp (exp1 exps)
                      (set! exp exp1)
                      (set! cont (begin-cont exps env cont))
                      (value-of/k))
           (else
            (eopl:error 'vaule-of/k "not implements"))
           )))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; usage : reads registers
;;     cont : Cont
;;     val  : ExpVal
;; Page 169 and 170
(define apply-cont
  (lambda ()
    (cases continuation cont

           (end-cont ()
                     (eopl:printf "End of computation.~%")
                     (set! pc #f)
                     val)
           ;; or (logged-print val)  ; if you use drscheme-init-cps.rkt
           (zero1-cont (saved-cont)
                       ;; (apply-cont cont
                       ;;   (bool-val
                       ;;     (zero? (expval->num val))))
                       (set! cont saved-cont)
                       (set! val (bool-val (zero? (expval->num val))))
                       (apply-cont))
           (let-exp-cont (var body saved-env saved-cont)
                         ;; (value-of/k body (extend-env id val env) cont)
                         (set! cont saved-cont)
                         (set! exp body)
                         (set! env (extend-env var (newref val) saved-env))
                         (value-of/k))
           (let-head-cont (vars exps body saved-env saved-cont)
                          (if (null? exps)
                              (begin
                                (set! exp body)
                                (set! env (extend-env (car vars) (newref val) saved-env))
                                (set! cont saved-cont)
                                (value-of/k))
                              (begin
                                (set! exp (let-exp (cdr vars) exps body))
                                (set! env (extend-env (car vars) (newref val) saved-env))
                                (set! cont saved-cont)
                                (value-of/k))))
           (let2-exp-cont (var1 var2 exp2 body saved-env saved-cont)
                          (let ([new-env (extend-env var1 (newref val) saved-env)]
                                [new-exp [let-exp var2 exp2 body]])
                            (set! exp new-exp)
                            (set! env new-env)
                            (set! cont saved-cont)
                            (value-of/k)))
           (let3-exp-cont (var1 var2 exp2 var3 exp3 body saved-env saved-cont)
                          (let ([new-env (extend-env var1 (newref val) saved-env)]
                                [new-exp (let2-exp var2 exp2 var3 exp3 body)])
                            (set! exp new-exp)
                            (set! env new-env)
                            (set! cont saved-cont)
                            (value-of/k)))
           (if-test-cont (exp2 exp3 saved-env saved-cont)
                         (set! cont saved-cont)
                         (if (expval->bool val)
                             (set! exp exp2)
                             (set! exp exp3))
                         (set! env saved-env)
                         (value-of/k))
           (diff1-cont (exp2 saved-env saved-cont)
                       ;; (value-of/k exp2 env (diff2-cont val cont)))
                       (set! cont (diff2-cont val saved-cont))
                       (set! exp exp2)
                       (set! env saved-env)
                       (value-of/k))
           (diff2-cont (val1 saved-cont)
                       ;; (apply-cont cont (num-val (- num1 num2)))))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val)))
                         (set! cont saved-cont)
                         (set! val (num-val (- num1 num2)))
                         (apply-cont)))
           (multi1-cont (exp2 saved-env saved-cont)
                        (set! exp exp2)
                        (set! env saved-env)
                        (set! cont (multi2-cont val saved-cont))
                        (value-of/k))
           (multi2-cont (val1 saved-cont)
                        (set! val (num-val
                                   (* (expval->num val1)
                                      (expval->num val))))
                        (set! cont saved-cont)
                        (apply-cont))
           (rator-cont (rands saved-env saved-cont)
                       (if (null? rands)
                           (begin
                             (set! cont saved-cont)
                             (set! proc1 (expval->proc val))
                             (set! val (list))
                             (set! pc apply-procedure/k))
                           ;; (apply-procedure/k))
                           (begin
                             (set! cont (rands-cont val (list) (cdr rands) saved-env saved-cont))
                             (set! exp (car rands))
                             (set! env saved-env)
                             (value-of/k))))
           (rands-cont (rator-val val2 tail-rands saved-env saved-cont)
                       (let ((rator-proc (expval->proc rator-val)))
                         (if (null? tail-rands)
                             (begin
                               (set! cont saved-cont)
                               (set! proc1 rator-proc)
                               (set! val (append val2 (list val)))
                               (set! pc apply-procedure/k))
                             ;; (apply-procedure/k))
                             (begin
                               (set! cont (rands-cont rator-val (append val2 (list val))
                                                      (cdr tail-rands) saved-env saved-cont))
                               (set! env saved-env)
                               (set! exp (car tail-rands))
                               (value-of/k)))))
           (cons-cont (tail saved-env saved-cont)
                      (if (equal? tail (emptylist-exp))
                          (begin (set! val (list-val (list val)))
                                 (set! cont saved-cont)
                                 (apply-cont))
                          (begin (set! exp tail)
                                 (set! env saved-env)
                                 (set! cont (cons2-cont val saved-cont))
                                 (value-of/k))))
           (cons2-cont (val1 saved-cont)
                       (set! val (list-val (cons val1 (expval->list val))))
                       (set! cont saved-cont)
                       (apply-cont))
           (car-cont (saved-cont)
                     (set! cont saved-cont)
                     (set! val (car (expval->list val)))
                     (apply-cont))
           (cdr-cont (saved-cont)
                     (set! cont saved-cont)
                     (set! val (list-val (cdr (expval->list val))))
                     (apply-cont))
           (null?-cont (saved-cont)
                       (set! cont saved-cont)
                       (set! val (bool-val (null? (expval->list val))))
                       (apply-cont))
           (lst-head-cont (tail saved-env saved-cont)
                          (if (null? tail)
                              (begin (set! val (list-val (cons val '())))
                                     (set! cont saved-cont)
                                     (apply-cont))
                              (begin (set! exp (list-exp tail))
                                     (set! env saved-env)
                                     (set! cont (lst-tail-cont val saved-cont))
                                     (value-of/k))))
           (lst-tail-cont (val1 saved-cont)
                          (set! cont saved-cont)
                          (set! val (list-val (cons val1 (expval->list val))))
                          (apply-cont))
           (set-rhs-cont (ref1 saved-cont)
                         (begin (setref! ref1 val)
                                (set! val (num-val 27))
                                (set! cont saved-cont)
                                (apply-cont)))
           (begin-cont (exps saved-env saved-cont)
                       (if (null? exps)
                           (begin (set! cont saved-cont)
                                  (apply-cont))
                           (begin (set! exp (car exps))
                                  (set! env saved-env)
                                  (set! cont (begin-cont (cdr exps) saved-env saved-cont))
                                  (value-of/k))))
           (else
            (eopl:error 'apply-cont "not implements"))
           )))

;; apply-procedure : Proc * ExpVal -> ExpVal
;; apply-procedure/k : () -> FinalAnswer}
;; usage : relies on registers
;;     proc1 : Proc
;;       val : ExpVal
;;      cont : Cont
;; Page 170
(define apply-procedure/k
  (lambda ()
    (when (trace-apply-procedure)
      (begin
        (eopl:printf
         "~%entering apply-procedure:~%proc1=~s~%val=~s~%cont=~s~%"
         proc1 val cont)))
    (cases proc proc1
           (procedure (vars body saved-env)
                      (set! exp body)
                      (set! env (extend-env* vars (map newref val) saved-env))
                      (value-of/k)))))

;; instrumented version
;; (define apply-procedure/k
;;   (lambda ()                       ; (proc1 val cont)
;;     (if (trace-apply-procedure)
;;       (begin
;;         (eopl:printf
;;           "~%entering apply-procedure:~%proc1=~s~%val=~s~%cont=~s~%"
;;           proc1 val cont)))
;;     (cases proc proc1
;;       (procedure (var body saved-env)
;;         (set! exp body)
;;         (set! env (extend-env var val saved-env))
;;         (value-of/k)))))

;; )
