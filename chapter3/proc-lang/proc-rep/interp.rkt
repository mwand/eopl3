#lang eopl


;; interpreter for the PROC language, using the procedural
;; representation of procedures.

;; The \commentboxes are the latex code for inserting the rules into
;; the code in the book. These are too complicated to put here, see
;; the text, sorry. 
(require (only-in racket
                  filter
                  flatten))

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
           ;; (let-exp (var exp1 body)
           ;;          (let ((val1 (value-of exp1 env)))
           ;;            (value-of body
           ;;                      (extend-env var val1 env))))
           (let-exp (vars exps body)
                    (let ((vals
                           (map
                            (lambda (e) (value-of e env))
                            exps
                            )))
                      (value-of body
                                (extend-env* vars vals env))))

           (proc-exp (vars body)
                     (let [(new-env (filter-env vars body env))]
                     (proc-val (procedure vars body new-env
                                          ))))
                     ;; (proc-val (procedure vars body env)))


           (letproc-exp (name vars func body)
                        (let [(procval (proc-val (traceproc vars func env)))]
                          (value-of body
                                    (extend-env name procval env))))

           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg
                            (map
                             (lambda (e) (value-of e env))
                             rand)))
                            ;; (value-of rand env)))
                       (apply-procedure proc arg)))
           )))


;; procedure : Var * Exp * Env -> Proc
;; Page: 79
(define procedure
  (lambda (vars body env)
    (lambda (vals)
      ;; (let [(new-env (extend-env* vars vals env))]
        ;; (begin (display new-env)
        ;;        (newline)
        ;;        (value-of body new-env))
        ;; ))))
      (value-of body (extend-env* vars vals env)))))

;; apply-procedure : Proc * ExpVal -> ExpVal
;; Page: 79
(define apply-procedure
  (lambda (proc vals)
    (proc vals)))


;; ex 3.27
(define traceproc
  (lambda (vars body env)
    (lambda (vals)
      (let [(v (value-of body (extend-env* vars vals env)))]
        (begin (eopl:printf "enter proc:~s~%" body)
               v
               (eopl:printf "leave proc:~s~%" body))
        v))))

;; ex3.26 only free var in env
;; occur-free? : Var * Env -> list-of Var
(define occur-free
  (lambda (search-var exp)
    (cases expression exp
           (var-exp (var)
                    (if (equal? var search-var)
                        '()
                        (list var)))
           (const-exp (num) '())
           (diff-exp (exp1 exp2)
                     (append (occur-free search-var exp1)
                             (occur-free search-var exp2)))
           (zero?-exp (exp1) (occur-free search-var exp1))
           (if-exp (exp1 exp2 exp3)
                   (append (occur-free search-var exp1)
                           (occur-free search-var exp2)
                           (occur-free search-var exp3)))
           (let-exp (var exps body)
                    (occur-free search-var body))
           (proc-exp (vars body)
                     (occur-free* (cons search-var vars) body))
           (letproc-exp (name vars func body)
                        (append (occur-free* (cons search-var vars) func)
                                (occur-free* (cons search-var vars) body)))
           (call-exp (rator rand)
                     (append (occur-free search-var rator)
                             (map (lambda (e)
                                    (occur-free search-var e)
                                    )
                                  rand)))
           )))

(define occur-free*
  (lambda (search-vars exp)
    (flatten
     (map (lambda (v) (occur-free v exp)) search-vars)
     )))

(define filter-env
  (lambda (vars body env)
    (let* [(want-vars (occur-free* vars body)) ;(flatten (free body)))
           (want-vals (filter (lambda (bind) (car bind))
                                (map (lambda (v)
                                       (bind-env env v))
                                     want-vars)))]
      ;; (let [(new-env (extend-env* want-vars
      ;;                             (map cadr want-vals) (empty-env)))]
      ;;   (begin (display "filter-env")
      ;;          (newline)
      ;;          (display new-env)
      ;;          (newline)
      ;;          (display want-vars)
      ;;          (newline)
      ;;          (display want-vals)
      ;;          (newline)
      ;;          new-env)))))
      (extend-env* want-vars
                   (map cadr want-vals)
                   (empty-env)))))
;; free : ExpVal -> Var
;; (define free
;;   (lambda (vars exp)
;;     (cases expression exp
;;            (var-exp (var)
;;                     (list var))
;;            (const-exp (num) '())
;;            (diff-exp (exp1 exp2)
;;                      (append (free exp1) (free exp2)))
;;            (zero?-exp (exp1) (free exp1))
;;            (if-exp (exp1 exp2 exp3)
;;                    (append (free exp1) (free exp2) (free exp3)))
;;            (let-exp (var exp1 body)
;;                     (free body))
;;                     ;; (cons var (append (free exp1) (free body))))
;;                     ;; (append (free exp1) (free body)))
;;            (proc-exp (vars body)
;;                      (append vars (free body)))
;;            (letproc-exp (name vars func body)
;;                         (append (free func) (free body)))
;;            (call-exp (rator rand)
;;                      (append (free rator)
;;                              (flatten (map free rand))))
;;                      ;; (free  rand))
;;                      ;; (append (rator rand)))
;;            )))

