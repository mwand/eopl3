#lang eopl
;; interpreter for the LEXADDR language.

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-translation value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-translation : Nameless-program -> ExpVal

(define value-of-translation
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-nameless-env))))))

;; value-of-translation : Nameless-program -> ExpVal
;; Page: 100
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-nameless-env))))))

;; value-of : Nameless-exp * Nameless-env -> ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
           (const-exp (num) (num-val num))

           (diff-exp (exp1 exp2)
                     (let ((val1
                            (expval->num
                             (value-of exp1 nameless-env)))
                           (val2
                            (expval->num
                             (value-of exp2 nameless-env))))
                       (num-val
                        (- val1 val2))))

           (zero?-exp (exp1)
                      (let ((val1 (expval->num (value-of exp1 nameless-env))))
                        (if (zero? val1)
                            (bool-val #t)
                            (bool-val #f))))

           (if-exp (exp0 exp1 exp2)
                   (if (expval->bool (value-of exp0 nameless-env))
                       (value-of exp1 nameless-env)
                       (value-of exp2 nameless-env)))

           (cond-exp (exp1 exp2)
                     (if (null? exp1)
                         (eopl:error "none of cond succeed")
                         (let ((condval (value-of (car exp1) nameless-env)))
                           (if (expval->bool condval)
                               (value-of (car exp2) nameless-env)
                               (value-of
                                (cond-exp (cdr exp1) (cdr exp2))
                                nameless-env
                                )))))

           (emptylist-exp () (list-val '()))

           (cons-exp (head tail)
                     (let* ([val1 (value-of head nameless-env)]
                            [val2 (value-of tail nameless-env)]
                            [num2 (expval->list  val2)])
                       (list-val
                        (cons val1 num2))))

           (car-exp (exp)
                    (let ((val1 (value-of exp nameless-env)))
                      (let ((num1 (expval->list val1)))
                        (car num1))))

           (cdr-exp (exp)
                    (let ((val1 (value-of exp nameless-env)))
                      (let ((num1 (expval->list val1)))
                        (list-val
                         (cdr num1)))))

           (list-exp (exp)
                     (list-val
                      (map
                       (lambda (exp1)
                         (value-of exp1 nameless-env))
                       exp)))

           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator nameless-env)))
                           (args (map
                                  (lambda (e) (value-of e nameless-env))
                                  rands)))
                       (apply-procedure proc args)))

           (nameless-var-exp (depth pos)
                             (apply-nameless-env nameless-env depth pos))


           (nameless-let-exp (exps body)
                             (let ([vals (map
                                         (lambda (e) (value-of e nameless-env))
                                         exps)])
                               (value-of body
                                         (extend-nameless-env vals nameless-env))))

           (nameless-letrec-var-exp (depth pos)
                                    (let ([proc1 (expval->proc (apply-nameless-env nameless-env depth pos))])
                                      (cases proc proc1
                                             (procedure (body saved-env)
                                                        (proc-val (procedure body
                                                                             (extend-nameless-env (proc-val proc1) saved-env)))))))

           (nameless-proc-exp (body)
                              (proc-val
                               (procedure body nameless-env)))

           (nameless-unpack-exp (lst body)
                                (let* [(lstval (expval->list (value-of lst nameless-env)))
                                       (new-env (extend-nameless-env lstval nameless-env))]
                                  (value-of body new-env)))

           (nameless-letrec-exp (p-body letrec-body)
                                (let* ([val (proc-val
                                             (procedure
                                              p-body
                                              nameless-env))]
                                       [new-env (extend-nameless-env val nameless-env)])
                                  (value-of letrec-body new-env)))
                                            ;; (extend-nameless-env val nameless-env))))

           (else
            (eopl:error 'value-of
                        "Illegal expression in translated code: ~s" exp))

           )))


;; apply-procedure : Proc * ExpVal -> ExpVal

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (body saved-env)
                      (value-of body (extend-nameless-env args saved-env))))))
