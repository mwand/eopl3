#lang eopl

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of/k)
(provide trace-apply-procedure)

(define trace-apply-procedure (make-parameter #f))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;


;; (define-datatype continuation continuation?
;;   (end-cont)                          ; []
;;   (diff1-cont                       ; cont[(- [] (value-of e2 env))]
;;    (exp2 expression?)
;;    (env environment?)
;;    (cont continuation?))
;;   (diff2-cont                         ; cont[(- val1 [])]
;;    (val1 expval?)
;;    (cont continuation?))
;;   (div1-cont                       ; cont[(- [] (value-of e2 env))]
;;    (exp2 expression?)
;;    (env environment?)
;;    (cont continuation?))
;;   (div2-cont                         ; cont[(- val1 [])]
;;    (val1 expval?)
;;    (env environment?)
;;    (cont continuation?))
;;   (unop-arg-cont
;;    (unop unary-op?)
;;    (cont continuation?))
;;   (if-test-cont
;;    (exp2 expression?)
;;    (exp3 expression?)
;;    (env environment?)
;;    (cont continuation?))
;;   (rator-cont            ; cont[(apply-proc [] (value-of rand env))]
;;    (rands (list-of expression?))
;;    (env environment?)
;;    (cont continuation?))
;;   (rands-cont                          ; cont[(apply-proc val1 [])]
;;    (val1 expval?)
;;    (val2 (list-of expval?))
;;    (rest (list-of expression?))
;;    (env environment?)
;;    (cont continuation?))
;;   (try-cont
;;    (var symbol?)
;;    (handler-exp expression?)
;;    (env environment?)
;;    (cont continuation?))
;;   (raise1-cont
;;    (saved-cont continuation?))
;;   )

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (value-of/k body (init-env) (end-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 173
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp

           (const-exp (num) (apply-cont cont (num-val num)))

           (const-list-exp (nums)
                           (apply-cont cont
                                       (list-val (map num-val nums))
                                       ))

           (var-exp (var) (apply-cont cont (apply-env env var) ))

           (diff-exp (exp1 exp2)
                     (value-of/k exp1 env
                                 (diff1-cont exp2 env cont)
                                 ))

           (div-exp (exp1 exp2)
                    (value-of/k exp1 env
                                (div1-cont exp2 env cont)
                                ))

           (unop-exp (unop exp1)
                     (value-of/k exp1 env
                                 (unop-arg-cont unop cont)
                                 ))

           (if-exp (exp1 exp2 exp3)
                   (value-of/k exp1 env
                               (if-test-cont exp2 exp3 env cont)
                               ))

           (proc-exp (vars body)
                     (apply-cont cont
                                 (proc-val
                                  (procedure vars body env))
                                 ))

           (call-exp (rator rand)
                     (value-of/k rator env
                                 (rator-cont rand env cont)
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
                                (try-cont var handler-exp env cont)
                                ))

           (raise-exp (exp1)
                      (value-of/k exp1 env
                                  (raise1-cont cont)
                                  )))))

;; apply-cont : continuation * expval -> final-expval

(define apply-cont
  (lambda (cont val)
    (cases expval val
           (cont-val (cont)
                     ;; (eopl:error "not implement"))
                     (apply-handler val cont))
           (else
            (cases continuation cont
                   (end-cont () val)
                   (diff1-cont (exp2 saved-env saved-cont)
                               (value-of/k exp2 saved-env (diff2-cont val saved-cont) ))
                   (diff2-cont (val1 saved-cont)
                               (let ((n1 (expval->num val1))
                                     (n2 (expval->num val)))
                                 (apply-cont saved-cont
                                             (num-val (- n1 n2))
                                             )))
                   (div1-cont (exp2 saved-env saved-cont)
                              (value-of/k exp2
                                          saved-env
                                          (div2-cont val saved-env saved-cont)
                                          ))
                   (div2-cont (val1 saved-env saved-cont)
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
                   (unop-arg-cont (unop cont)
                                  (apply-cont cont
                                              (apply-unop unop val)
                                              ))
                   (if-test-cont (exp2 exp3 env cont)
                                 (if (expval->bool val)
                                     (value-of/k exp2 env cont )
                                     (value-of/k exp3 env cont )))
                   ;; (rator-cont (rands saved-env saved-cont)
                   ;;             (value-of/k rand saved-env
                   ;;                         (rand-cont val saved-cont)
                   ;;                         ))
                   ;; (rands-cont (val1 saved-cont)
                   ;;            (let ((proc (expval->proc val1)))
                   ;;              (apply-procedure proc val saved-cont )))
                   (rator-cont (rands saved-env saved-cont)
                               (if (null? rands)
                                   (apply-procedure (expval->proc val)
                                                    (list) saved-cont
                                                    )
                                   (value-of/k (car rands) saved-env
                                               (rands-cont val (list) (cdr rands) saved-env saved-cont)
                                               )))
                   (rands-cont (val1 val2 tail-rands saved-env saved-cont)
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
                                                             (cdr tail-rands) saved-env saved-cont)
                                                 ))))
                   ;; the body of the try finished normally-- don't evaluate the handler
                   (try-cont (var handler-exp saved-env saved-cont)
                             (apply-cont saved-cont val ))
                   ;; val is the value of the argument to raise
                   (raise1-cont (saved-cont)
                                ;; we put the short argument first to make the trace more readable.
                                ;; (apply-handler val saved-cont ))
                                (apply-cont saved-cont
                                            (cont-val (raise2-cont val saved-cont))))
                   (raise2-cont (val saved-cont)
                                (apply-handler val saved-cont))
                   )))))

;; apply-handler : ExpVal * Cont -> FinalAnswer
(define apply-handler
  (lambda (val cont)
    (cases continuation cont
           ;; interesting cases
           (try-cont (var handler-exp saved-env saved-cont)
                     (value-of/k handler-exp
                                 (extend-env var val saved-env)
                                 ;; saved-cont
                                 ;; resume
                                 saved-cont))

           (end-cont () (eopl:error 'apply-handler "uncaught ion!"))

           ;; otherwise, just look for the handler...
           ;; (else
           ;;  (eopl:error "not a ion cont:~s~%" ))
           (diff1-cont (exp2 saved-env saved-cont)
                       (apply-handler val saved-cont))
           (diff2-cont (val1 saved-cont)
                       (apply-handler val saved-cont))
           (div1-cont (exp2 saved-env saved-cont)
                       (apply-handler val saved-cont))
           (div2-cont (val1 saved-env saved-cont)
                       (apply-handler val saved-cont))
           (if-test-cont (exp2 exp3 env saved-cont)
                         (apply-handler val saved-cont))
           (unop-arg-cont (unop saved-cont)
                          (apply-handler val saved-cont))
           (rator-cont (rand saved-env saved-cont)
                       (apply-handler val saved-cont))
           (rands-cont (val1 val2 tails-rands saved-env saved-cont)
                      (apply-handler val saved-cont))
           (raise1-cont (cont)
                        (apply-handler val cont))
           (raise2-cont (val1 cont)
                        (apply-handler val1 cont))
           )))


;; apply-procedure : procedure * expval * cont -> final-expval

(define apply-procedure
  (lambda (proc1 args cont )
    (cases proc proc1
           (procedure (vars body saved-env)
                      (if (eq? (length vars) (length args))
                          (value-of/k body
                                      (extend-env* vars args saved-env)
                                      cont
                                      )
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
(trace value-of/k apply-cont apply-handler)
;; (trace value-of/k)

