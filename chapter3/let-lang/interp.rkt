#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

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
                              (/ num1 num2)))))

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
           (let-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var val1 env))))

           ;\commentbox{\ma{\minusspec}}
           (minus-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (num-val
                         (- 0 (expval->num val1)))))

           (null?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (bool-val
                         (null? (expval->list val1)))))

           (cons-exp (head tail)
                     (let ((val1 (value-of head env))
                           (val2 (value-of tail env)))
                       (let ((num2 (expval->list  val2)))
                         (list-val
                          (cons val1 num2)))))

           (car-exp (li)
                     (let ((val1 (value-of li env)))
                       (let ((num1 (expval->list val1)))
                          (car num1))))

           (cdr-exp (li)
                    (let ((val1 (value-of li env)))
                      (let ((num1 (expval->list val1)))
                        (list-val
                         (cdr num1)))))

           (emptylist-exp (list-val '()))
           )))

