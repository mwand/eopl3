#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of value-of-print)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

;; value-of-print: Print->ExpVal
(define value-of-print
  (lambda (exp)
    (display exp)
    (num-val 1)))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
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

           (addition-exp (exp1 exp2)
                         (let ((val1 (value-of exp1 env))
                               (val2 (value-of exp2 env)))
                           (let ((num1 (expval->num val1))
                                 (num2 (expval->num val2)))
                             (num-val
                              (+ num1 num2)))))

           ;\commentbox{\ma{\minusspec}}
           (minus-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (num-val
                         (- 0 (expval->num val1)))))

           (multiplication-exp (exp1 exp2)
                               (let ((val1 (value-of exp1 env))
                                     (val2 (value-of exp2 env)))
                                 (let ((num1 (expval->num val1))
                                       (num2 (expval->num val2)))
                                   (num-val
                                    (* num1 num2)))))

           (quotient-exp (exp1 exp2)
                        (let ((val1 (value-of exp1 env))
                               (val2 (value-of exp2 env)))
                           (let ((num1 (expval->num val1))
                                 (num2 (expval->num val2)))
                             (num-val
                              (quotient num1 num2)))))

           ;\commentbox{\zerotestspec}
           (zero?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1)))
                          (if (zero? num1)
                              (bool-val #t)
                              (bool-val #f)))))

           (equal?-exp (exp1 exp2)
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
                         (let ((num1 (expval->num val1))
                               (num2 (expval->num val2)))
                           (if (zero?
                                 (- num1 num2))
                               (bool-val #t)
                               (bool-val #f))))) 

           (greater?-exp (exp1 exp2)
                         (let ((val1 (value-of exp1 env))
                               (val2 (value-of exp2 env)))
                           (let ((num1 (expval->num val1))
                                 (num2 (expval->num val2)))
                             (bool-val
                              (> num1 num2)))))

           (less?-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val2)))
                          (bool-val
                           (< num1 num2)))))


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
                                    (extend-env-list-val vars vals env))))

           (let*-exp (vars exps body)
                     (let
                         ((new-env (extend-env-list-exps vars exps env)))
                         (value-of body new-env)))

           (null?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (bool-val
                         (null? (expval->list val1)))))

           ;\\commentbox{\ma{\thelistspec}}
           (emptylist-exp () (list-val '()))

           (cons-exp (head tail)
                     (let ((val1 (value-of head env))
                           (val2 (value-of tail env)))
                       (let ((num2 (expval->list  val2)))
                         (list-val
                          (cons val1 num2)))))

           (car-exp (exp)
                     (let ((val1 (value-of exp env)))
                       (let ((num1 (expval->list val1)))
                          (car num1))))

           (cdr-exp (exp)
                    (let ((val1 (value-of exp env)))
                      (let ((num1 (expval->list val1)))
                        (list-val
                         (cdr num1)))))

           (list-exp (exp)
                     (list-val
                      (map
                       (lambda (exp1)
                         (value-of exp1 env))
                       exp)))

           (unpack-exp (syms lst body)
                       (let* [(lstval (expval->list (value-of lst env)))
                              (new-env (extend-env-list-val syms lstval env))]
                         (value-of body new-env)))

           ;\\commentbox{\ma\thecondspec}
           (cond-exp (exp1 exp2)
                     (if (null? exp1)
                         (eopl:error "none of cond succeed")
                         (let ((condval (value-of (car exp1) env)))
                           (if condval
                               (value-of (car exp2) env)
                               (cond-exp (cdr exp1) (cdr exp2))))))
           )))

;; for let, ex 3.16 3.17
;; https://docs.racket-lang.org/reference/let.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._letrec%29%29
(define (extend-env-list-val vars vals env)
  (if (or (null? vars) (null? vals))
      env
      (extend-env-list-val (cdr vars) (cdr vals)
                           (extend-env (car vars) (car vals) env)))
  )

;; helper func for let*, ex 3.17
(define (extend-env-list-exps vars exps env)
  (if (or (null? vars) (null? exps))
      env
      (extend-env-list-exps (cdr vars) (cdr exps)
                            (extend-env (car vars) (value-of (car exps) env) env))))
