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
(require "store.rkt")

(provide value-of-program value-of/k instrument-newref instrument-cont)


(define instrument-cont (make-parameter #f))
(define cont-max-depth 0)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> FinalAnswer
;; Page: 143 and 154
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (set! cont-max-depth 0)
    (cases program pgm
           (a-program (exp1)
                      (trampoline
                       (value-of/k exp1 (init-env) (end-cont)))))))

;; Bounce = ExpVal ∪ (() → Bounce)
;; trampoline : Bounce → FinalAnswer
(define trampoline
  (lambda (bounce)
    (if (expval? bounce)
        bounce
        (trampoline (bounce)))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; value-of/k : Exp * Env * Cont → Bounce
;; Page: 143--146, and 154
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           (const-exp (num) (apply-cont cont (num-val num)))
           (var-exp (var) (apply-cont cont (deref (apply-env env var))))
           (proc-exp (vars body)
                     (apply-cont cont
                                 (proc-val (procedure vars body env))))
           (letrec-exp (p-name b-var p-body letrec-body)
                       (value-of/k letrec-body
                                   (extend-env-rec* p-name b-var p-body env)
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
           (multi-exp (exp1 exp2)
                      (value-of/k exp1 env
                                  (multi1-cont exp2 env cont)))
           (call-exp (rator rands)
                     (value-of/k rator env
                                 (rator-cont rands env cont)))
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
           (assign-exp (var exp1)
                       (let ([ref (apply-env env var)])
                         (value-of/k exp1 env
                                     (set-rhs-cont ref cont))))

           (begin-exp (exp1 exps)
                      (value-of/k exp1 env
                                  (begin-cont exps env cont)))
           )))

;; value-of/k* : (listof Exp) * Env * Cont -> (list-of ExpVal)
;; error implements, because end-cont will call on every vals
;; (define value-of/k*
;;   (lambda (exps env cont)
;;     (let* ([value-of/k-curry (lambda (e) (value-of/k e env cont))]
;;            [vals (map value-of/k-curry exps)])
;;       vals)))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; apply-cont : Cont * ExpVal -> Bounce
;; Page: 148
(define apply-cont
  (lambda (cont val)
    (when (instrument-cont)
      (let ([d (continuation-depth cont)])
        (when (> d cont-max-depth)
          (begin (set! cont-max-depth d)
                 (eopl:printf
                  "apply-cont: ~s~%"
                  cont)))))
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
                                     (extend-env var (newref val) saved-env) saved-cont))
           (let-head-cont (vars exps body saved-env saved-cont)
                          (if (null? exps)
                              (value-of/k body
                                          (extend-env (car vars) (newref val) saved-env)
                                          saved-cont)
                              (value-of/k (let-exp (cdr vars) exps body)
                                          (extend-env (car vars) (newref val) saved-env)
                                          saved-cont)))
           (let2-exp-cont (var1 var2 exp2 body saved-env saved-cont)
                          (let ([new-env (extend-env var1 (newref val) saved-env)]
                                [new-exp [let-exp var2 exp2 body]])
                            (value-of/k new-exp new-env saved-cont)))
           (let3-exp-cont (var1 var2 exp2 var3 exp3 body saved-env saved-cont)
                          (let ([new-env (extend-env var1 (newref val) saved-env)]
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
           (multi1-cont (exp2 saved-env saved-cont)
                        (value-of/k exp2 saved-env
                                    (multi2-cont val saved-cont)))
           (multi2-cont (val1 saved-cont)
                        (apply-cont saved-cont
                                    (num-val
                                     (* (expval->num val1)
                                        (expval->num val)))))
           (rator-cont (rands saved-env saved-cont)
                       (if (null? rands)
                           (apply-procedure/k (expval->proc val)
                                              (list) saved-cont)
                           (value-of/k (car rands) saved-env
                                       (rands-cont val (list) (cdr rands) saved-env saved-cont))))
           (rands-cont (val1 val2 tail-rands saved-env saved-cont)
                      (let ([proc (expval->proc val1)])
                        (if (null? tail-rands)
                            (apply-procedure/k proc (append val2 (list val)) saved-cont)
                            (value-of/k (car tail-rands) saved-env
                                        (rands-cont val1
                                                    (append val2 (list val))
                                                    (cdr tail-rands) saved-env saved-cont)))))
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
           (set-rhs-cont (ref1 saved-cont)
                         ;; (begin (setref! (apply-env saved-env var1) val)
                         (begin (setref! ref1 val)
                                (apply-cont saved-cont (num-val 27))))
           (begin-cont (exps saved-env saved-cont)
                       (if (null? exps)
                           (apply-cont saved-cont val)
                           (value-of/k (car exps) saved-env
                                       (begin-cont (cdr exps) saved-env saved-cont))))
           )))

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
;; apply-procedure/k : Proc * ExpVal * Cont -> Bounce
;; Page 152 and 155
(define apply-procedure/k
  (lambda (proc1 args cont)
    (lambda ()
      (cases proc proc1
             (procedure (vars body saved-env)
                        (value-of/k body
                                    (extend-env* vars (map newref args) saved-env)
                                    cont))))))



(define continuation-depth
  (lambda (cont)
  (cases continuation cont
         (end-cont () 1)
         (zero1-cont (saved-cont)
                     (+ 1 (continuation-depth saved-cont)))
         (let-exp-cont (var body env saved-cont)
                       (+ 1 (continuation-depth saved-cont)))
         (let-head-cont (vars exps body env saved-cont)
                        (+ 1 (continuation-depth saved-cont)))
         (let2-exp-cont (v1 v2 e2 b saved-env saved-cont)
                        (+ 1 (continuation-depth saved-cont)))
         (let3-exp-cont (v1 v2 e2 v3 e3 b e saved-cont)
                        (+ 1 (continuation-depth saved-cont)))
         (if-test-cont (e2 e3 e saved-cont)
                       (+ 1 (continuation-depth saved-cont)))
         (diff1-cont (e2 e saved-cont)
                     (+ 1 (continuation-depth saved-cont)))
         (diff2-cont (v1 saved-cont)
                     (+ 1 (continuation-depth saved-cont)))
         (multi1-cont (e2 e saved-cont)
                      (+ 1 (continuation-depth saved-cont)))
         (multi2-cont (v1 saved-cont)
                      (+ 1 (continuation-depth saved-cont)))
         (rator-cont (v1 e saved-cont)
                     (+ 1 (continuation-depth saved-cont)))
         (rands-cont (v1 v2 t e saved-cont)
                     (+ 1 (continuation-depth saved-cont)))
         (cons-cont (t e saved-cont)
                    (+ 1 (continuation-depth saved-cont)))
         (lst-head-cont (t e saved-cont)
                        (+ 1 (continuation-depth saved-cont)))
         (lst-tail-cont (v1 saved-cont)
                        (+ 1 (continuation-depth saved-cont)))
         (set-rhs-cont (r saved-cont)
                       (+ 1 (continuation-depth saved-cont)))
         (begin-cont (es e saved-cont)
                     (+ 1 (continuation-depth saved-cont)))
         (else
          (eopl:error 'continuation
                      "not a continuation: ~s"
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

;; for debug propose
;; (define exp-of-program
;;   (lambda (pgm)
;;     (cases program pgm
;;            (a-program (exp1)
;;                       exp1))))

