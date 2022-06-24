#lang eopl

(require (only-in racket case-lambda))

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of/k)
(provide trace-apply-procedure)

(define trace-apply-procedure (make-parameter #f))
(define exception (make-parameter #f))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (value-of/k body (init-env) (end-cont) )))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 173
(define value-of/k
  (lambda (exp env cont )
    (cases expression exp

           (const-exp (num) (apply-cont cont (num-val num)))

           (const-list-exp (nums)
                           (apply-cont cont
                                       (list-val (map num-val nums))
                                       ))

           (var-exp (var) (apply-cont cont (apply-env env var)))

           (diff-exp (exp1 exp2)
                     (value-of/k exp1 env
                                 (diff1-cont exp2 env cont )
                                 ))

           (div-exp (exp1 exp2)
                    (value-of/k exp1 env
                                (div1-cont exp2 env cont )
                                ))

           (unop-exp (unop exp1)
                     (value-of/k exp1 env
                                 (unop-arg-cont unop cont )
                                 ))

           (if-exp (exp1 exp2 exp3)
                   (value-of/k exp1 env
                               (if-test-cont exp2 exp3 env cont )
                               ))

           (proc-exp (vars body)
                     (apply-cont cont
                                 (proc-val
                                  (procedure vars body env))
                                 ))

           (call-exp (rator rand)
                     (value-of/k rator env
                                 (rator-cont rand env cont )
                                 ))

           ;; make let a macro, because I'm too lazy to add the extra
           ;; continuation
           (let-exp (var exp1 body)
                    (value-of/k
                     (call-exp (proc-exp var body) exp1)
                     env
                     cont
                     ))

           (letrec-exp (p-name b-var p-body letrec-body)
                       (value-of/k
                        letrec-body
                        (extend-env-rec* p-name b-var p-body env)
                        cont
                        ))


           (try-exp (exp1 var handler-exp)
                    (value-of/k exp1 env
                                (try-cont var handler-exp env cont )
                                ))

           (raise-exp (exp1)
                      (value-of/k exp1 env
                                  (raise1-cont cont )
                                  ))

           (letcc-exp (var body)
                      ;; (eopl:printf "cont is ~s~%" cont)
                      (value-of/k body
                                  (extend-env var (cont-val cont) env)
                                  cont))

           (throw-exp (exp1 exp2)
                      (value-of/k exp1 env
                                  (throw1-cont exp2 env cont)))

           )))

;; apply-cont : continuation * expval -> final-expval
(define end-cont
  (lambda ()
    (case-lambda
      ((val)
       (begin (eopl:printf "End of computation. ~%")
              val))
      (()
       (eopl:error 'apply-handler "uncaught ion!"))
      )))

(define if-test-cont
  (lambda (exp2 exp3 saved-env saved-cont )
    (case-lambda
      ((val)
       (if (expval->bool val)
           (value-of/k exp2 saved-env saved-cont )
           (value-of/k exp3 saved-env saved-cont )))
      (()
       (apply-handler saved-cont )))))

(define diff1-cont
  (lambda (exp2 saved-env saved-cont )
    (case-lambda
      ((val)
       (value-of/k exp2
                   saved-env
                   (diff2-cont val saved-cont )
                   ))
      (()
       (apply-handler saved-cont )))))

(define diff2-cont
  (lambda (val1 saved-cont )
    (case-lambda
      ((val)
       (apply-cont saved-cont
                   (num-val (- (expval->num val1)
                               (expval->num val)))))
      (()
       (apply-handler saved-cont )))))

(define div1-cont
  (lambda (exp2 saved-env saved-cont )
    (case-lambda
      ((val)
       (value-of/k exp2
                   saved-env
                   (div2-cont val saved-env saved-cont)
                   ))
      (()
       (apply-handler saved-cont )))))

(define div2-cont
  (lambda (val1 saved-env saved-cont )
    (case-lambda
      ((val)
       (let ((n1 (expval->num val1))
             (n2 (expval->num val)))
         (if (zero? n2)
             (value-of/k (raise-exp (const-exp 98))
                         saved-env
                         saved-cont
                         )
             (apply-cont saved-cont
                         (num-val (/ n1 n2))
                         ))))
      (()
       (apply-handler saved-cont )))))

(define unop-arg-cont
  (lambda (unop cont )
    (case-lambda
      ((val)
       (apply-cont cont
                   (apply-unop unop val)))
      (()
       (apply-handler cont )))))

(define rator-cont
  (lambda (rands saved-env saved-cont )
    (case-lambda
      ((val)
       (if (null? rands)
           (apply-procedure (expval->proc val)
                            (list) saved-cont
                            )
           (value-of/k (car rands) saved-env
                       (rands-cont val (list) (cdr rands) saved-env saved-cont )
                       )))
      (()
       (apply-handler saved-cont )))))

(define rands-cont
  (lambda (val1 val2 tail-rands saved-env saved-cont )
    (case-lambda
      ((val)
       (let ([proc (expval->proc val1)])
         (if (null? tail-rands)
             (apply-procedure proc
                              (append val2 (list val))
                              saved-cont
                              )
             (value-of/k (car tail-rands)
                         saved-env
                         (rands-cont val1
                                     (append val2 (list val))
                                     (cdr tail-rands) saved-env saved-cont )
                         ))))
      (()
       (apply-handler saved-cont)))))

;; the body of the try finished normally-- don't evaluate the handler
(define try-cont
  (lambda (var handler-exp saved-env saved-cont)
    (case-lambda
      ((val)
       (apply-cont saved-cont val))
      (()
       (value-of/k handler-exp
                   (extend-env var (exception) saved-env)
                   saved-cont
                   )))))

;; val is the value of the argument to raise
(define raise1-cont
  (lambda (saved-cont)
    (lambda (val)
      ;; we put the short argument first to make the trace more readable.
      ;; (apply-handler val saved-cont))))
      (exception val)
      (apply-handler saved-cont))))

;; ref1 https://stackoverflow.com/questions/40141905/understanding-let-cc-and-throw-in-racket
;; ref2 https://docs.racket-lang.org/reference/cont.html
(define throw1-cont
  (lambda (exp2 saved-env saved-cont)
    (case-lambda
      ((val)
       ;; (eopl:printf "exp2 is ~s, val is ~s~%" exp2 val)
       ;; (apply-cont (expval->cont exp2) val)
       (value-of/k exp2 saved-env
                   (throw2-cont val))
       )
      (()
       (apply-handler saved-cont))
      )))

(define throw2-cont
  (lambda (val1)
    (case-lambda
      ((val)
       (cases expval val
              (cont-val (k)
                        (apply-cont k val1))
              (else
               (eopl:error 'throw2-cont "not a continuation val"))))
      (()
       (apply-handler (end-cont)))
      )))

(define apply-cont
  (lambda (cont val)
    (cont val)))

;; apply-handler : ExpVal * Cont -> FinalAnswer

(define apply-handler
  (lambda (cont)
    (cont)))


;; apply-procedure : procedure * expval * cont -> final-expval

(define apply-procedure
  (lambda (proc1 args cont )
    (cases proc proc1
           (procedure (vars body saved-env)
                      (if (eq? (length vars) (length args))
                          (value-of/k body
                                      (extend-env* vars args saved-env)
                                      cont)
                          (value-of/k (raise-exp (const-exp 98)) saved-env cont ))))))


(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
           (null?-unop ()
                       (bool-val
                        (null? (expval->list val))))
           (car-unop ()
                     (car (expval->list val)))
           (cdr-unop ()
                     (list-val (cdr (expval->list val))))
           (zero?-unop ()
                       (bool-val
                        (zero? (expval->num val)))))))


;; to get the detailed trace:
;; (trace value-of/k apply-cont apply-handler)
;; (trace value-of/k)

