#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "data-structures.rkt")
(require "store.rkt")
(require "environments.rkt")
(require "interp.rkt")

(provide result-of-program result-of)

(define result-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
           (a-program (stmt1)
                      (result-of stmt1 (init-env) (get-store))
                      (values)))))


(define result-of
  (lambda (stmt env sto)
    (cases statement stmt
           (assign-stmt (var exp1)
                        (value-of (assign-exp var exp1) env)
                        sto)

           (print-stmt (exp)
                       (display (value-of exp env))
                       (newline)
                       sto)

           (block-stmt (stmts)
                       (letrec ([result-of-block
                                 (lambda (stmts sto)
                                   (cond
                                     [(null? stmts) sto]
                                     [(null? (cdr stmts))
                                      (result-of (car stmts) env sto)]
                                     [else
                                      (result-of-block
                                       (cdr stmts)
                                       (result-of (car stmts) env sto))]))])
                         ;; (begin (display stmts)
                         ;;        (newline))
                         (result-of-block stmts sto)))

           (if-stmt (exp1 stmt1 stmt2)
                    (let ([val1 (value-of exp1 env)])
                      (if (expval->bool val1)
                          (result-of stmt1 env sto)
                          (result-of stmt2 env sto))))

           (while-stmt (exp1 stmt1)
                       (let ([val1 (value-of exp1 env)])
                         (if (expval->bool val1)
                             (begin
                               (result-of stmt1 env sto)
                               (result-of (while-stmt exp1 stmt1) env sto))
                             sto)))

           (declare-stmt (vars stmt1)
                         (result-of stmt1
                                    (extend-env*
                                     vars
                                     (map newref vars) ;; trick here, since vars aren't expvals
                                     env)
                                    sto))

           )))
