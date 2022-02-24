#lang eopl

(require (only-in racket filter flatten))

(require "lang.rkt")
(require "trimmer.rkt")

(provide translation-of-program has-binding?)
;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

;; translation-of-program : Program -> Nameless-program
;; Page: 96
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (a-program
                       (translation-of exp1 (init-senv)))))))
                        ;; (filter (occurs-free?? exp1) '(i v x)) (init-senv))))))


;; translation-of : Exp * Senv -> Nameless-exp
;; Page 97
(define translation-of
  (lambda (exp senv)
    (let ([translation-of* (lambda (e)
                             (translation-of e senv))])
      (cases expression exp
             (const-exp (num) (const-exp num))
             (diff-exp (exp1 exp2)
                       (diff-exp
                        (translation-of exp1 senv)
                        (translation-of exp2 senv)))
             (zero?-exp (exp1)
                        (zero?-exp
                         (translation-of exp1 senv)))
             (if-exp (exp1 exp2 exp3)
                     (if-exp
                      (translation-of exp1 senv)
                      (translation-of exp2 senv)
                      (translation-of exp3 senv)))
             (var-exp (var)
                      (let* ([val (apply-senv senv var)]
                             [depth (car val)]
                             [pos (cadr val)]
                             [letrec? (cddr val)])
                        (if letrec?
                            (nameless-letrec-var-exp depth pos)
                            (nameless-var-exp depth pos))))

             (let-exp (vars exps body)
                      (let* ([procs (filter (lambda (elem) (proc-exp? (cdr elem)))
                                            (map cons vars exps))]
                             [inlines (filter (lambda (elem) (can-inline? (car elem) body))
                                              procs)]
                             [new-body (inline-of* (map car inlines) (map cdr inlines) body)]
                             [new-vars (difference vars (map car inlines))]
                             [new-exps (difference exps (map cdr inlines))]
                             )
                        (begin (newline)
                               (display inlines)
                               (newline)
                               (display new-vars)
                               (newline)
                               ;; (display new-body)
                               (newline))
                        (if (null? new-vars)
                            (translation-of new-body senv)
                            (nameless-let-exp
                             (map (lambda (e) (translation-of e senv)) new-exps)
                             (translation-of new-body
                                             (extend-senv new-vars #f senv))))))

             (proc-exp (vars body)
                       (let* ([want-vars (lookup-free-vars vars body)]
                              [bind-vars (filter (lambda (v) (has-binding? v senv)) want-vars)]
                              [addrs (map (lambda (v) (apply-senv senv v)) bind-vars)]
                              [depths (map (lambda (addr) (car addr)) addrs)]
                              [postions (map (lambda (addr) (cadr addr)) addrs)]
                              [new-senv (if (null? want-vars)
                                            (extend-senv vars #f (empty-senv))
                                            (extend-senv vars #f
                                                         (extend-senv want-vars #f
                                                                      (empty-senv))))])
                         (nameless-proc-exp
                          depths
                          postions
                          (translation-of body new-senv))))

             (call-exp (rator rands)
                       (call-exp
                        (translation-of rator senv)
                        (map translation-of* rands)))

             (cond-exp (exps1 exps2)
                       (cond-exp
                        (map translation-of* exps1)
                        (map translation-of* exps2)))

             (emptylist-exp ()
                            (emptylist-exp))

             (cons-exp (head tail)
                       (cons-exp
                        (translation-of head senv)
                        (translation-of tail senv)))

             (car-exp (exp1)
                      (car-exp
                       (translation-of exp1 senv)))

             (cdr-exp (exp1)
                      (cdr-exp
                       (translation-of exp1 senv)))

             (list-exp (exps1)
                       (list-exp
                        (map translation-of* exps1)))

             (unpack-exp (syms lst body)
                         (nameless-unpack-exp
                          (translation-of lst senv)
                          (translation-of body
                                          (extend-senv syms #f senv))))

             (letrec-exp (p-names b-vars p-bodies letrec-body)
                         (nameless-letrec-exp
                          (map (lambda (vars body)
                                 (translation-of body
                                                 (extend-senv vars #f
                                                              (extend-senv p-names #t senv))))
                               b-vars
                               p-bodies)
                          (translation-of letrec-body
                                          (extend-senv p-names #t senv))))

             (else (report-invalid-source-expression exp))
             ))))

(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error 'value-of
                "Illegal expression in source code: ~s" exp)))

;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;

;;; Senv = Listof(Listof(Sym),Bool)
;;; Lexaddr = N

(define-datatype senv senv?
  (empty-senv)
  ;; (extend-senv1
  ;;  (var symbol?)
  ;;  (saved-env senv?))
  (extend-senv
   (svars (list-of symbol?))
   (letrec? boolean?)
   (saved-env senv?)))

(define apply-senv
  (lambda (senv1 search-var)
    (cases senv senv1
           (empty-senv ()
                       (report-unbound-var search-var))
           (extend-senv (vars letrec? saved-env)
                        (let loop ([vars vars]
                                   [depth 0]
                                   [pos 0])
                          (cond
                            [(null? vars)
                             (add1-first (apply-senv saved-env search-var))]
                            [(eqv? (car vars) search-var)
                             (cons depth (cons pos letrec?))]
                            [else
                             (loop (cdr vars) depth (+ 1 pos))]))))))

(define extend-senv*
  (lambda (lst-vars letrec? senv)
    (if (null? lst-vars)
        senv
        (extend-senv* (cdr lst-vars) letrec?
                      (extend-senv (car lst-vars) letrec? senv)))))

(define has-binding?
  (lambda (sym senv1)
    (cases senv senv1
           (empty-senv () #f)
           (extend-senv (vars letrec? saved-env)
                        (let loop ([vars vars])
                          (cond
                            [(null? vars)
                             (has-binding? sym saved-env)]
                            [(eqv? sym (car vars)) #t]
                            [else
                             (loop (cdr vars))]))))))

;; helper for apply-senv, add first
(define add1-first
  (lambda (elem)
  (cons (+ 1 (car elem)) (cdr elem))))

(define report-unbound-var
  (lambda (var)
    (eopl:error 'translation-of "unbound variable in code: ~s" var)))

;; init-senv : () -> Senv
;; Page: 96
(define init-senv
  (lambda ()
    (extend-senv '(i) #f
                 (extend-senv '(v) #f
                              (extend-senv '(x) #f
                                           (empty-senv))))))

;; for debug propose
(define get-program-body
  (lambda (pgm)
    (cases program (scan&parse pgm)
           (a-program (exp1) exp1))))

