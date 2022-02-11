#lang eopl

(require "lang.rkt")

(provide translation-of-program)
;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

;; translation-of-program : Program -> Nameless-program
;; Page: 96
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (a-program
                       (translation-of exp1 (init-senv)))))))

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
             ;; (nameless-var-exp
             ;;  (apply-senv senv var)))
             (let-exp (vars exps body)
                      (nameless-let-exp
                       (map (lambda (e) (translation-of e senv)) exps)
                       (translation-of body
                                       (extend-senv vars #f senv))))
             (proc-exp (vars body)
                       (nameless-proc-exp
                        (translation-of body
                                        (extend-senv vars #f senv))))
             (call-exp (rator rands)
                       (call-exp
                        (translation-of rator senv)
                         ;; (map (lambda (e) (translation-of e senv)) rands)))
                        (map translation-of* rands)))
                        ;; (translation-of rands senv)))

             (cond-exp (exp1 exp2)
                       (cond-exp
                        (map translation-of* exp1)
                        (map translation-of* exp2)))

             (emptylist-exp ()
                            (emptylist-exp))

             (cons-exp (head tail)
                       (cons-exp
                        (translation-of head senv)
                        (translation-of tail senv)))

             (car-exp (exp)
                      (car-exp
                       (translation-of exp senv)))

             (cdr-exp (exp)
                      (cdr-exp
                       (translation-of exp senv)))

             (list-exp (exp)
                       (list-exp
                        (map translation-of* exp)))

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
                          ;; (translation-of p-bodies
                          ;;                 (extend-senv* b-vars #f
                          ;;                               (extend-senv p-names #t senv)))
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
  ;; (extend-senv-rec
  ;;  (svars (list-of (list-of symbol?)))
  ;;  (letrec? boolean?)
  ;;  (saved-env senv?)))

(define apply-senv
  (lambda (senv1 search-var)
    (cases senv senv1
           (empty-senv ()
                       (report-unbound-var search-var))
           ;; (extend-senv1 (var saved-env)
           ;;               (if (eqv? var search-var)
           ;;                   (cons 0 (cons 0 #f))
           ;;                   (add1-first (extend-senv1 var (cdr saved-env)))))
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
           ;; (extend-senv-rec (lst-vars letrec? saved-env)
           ;;                  (let loop ([lst lst-vars]
           ;;                             [depth 0]
           ;;                             [pos 0])
           ;;                    (cond
           ;;                      [(null? lst)
           ;;                       (add1-first (apply-senv saved-env search-var))]
           ;;                      [(eqv? (caar lst) search-var)
           ;;                       ]
           ;;                    )))))

;; empty-senv : () -> Senv
;; Page: 95
(define empty-senv-1
  (lambda ()
    '()))

;; extend-senv : Var * Senv -> Senv
;; Page: 95
(define extend-senv-1
  (lambda (var letrec? senv)
    (if (list? var)
        (cons (cons var letrec?) senv)
        (cons (cons (list var) letrec?) senv))))

(define extend-senv*
  (lambda (lst-vars letrec? senv)
    (if (null? lst-vars)
        senv
        (extend-senv* (cdr lst-vars) letrec?
                      (extend-senv (car lst-vars) letrec? senv)))))
;; (define extend-senv*
;;   (lambda (vars senv)
;;     (if (null? vars)
;;         senv
;;         (extend-senv* (cdr vars)
;;                       (extend-senv (car vars) #f senv)))))

;; apply-senv : Senv * Var -> (Lexaddr, Letrec-var?)
;; Page: 95
(define apply-senv-1
  (lambda (senv var)
    (let loop ([senv senv]
               [depth 0]
               [pos 0])
      (if (null? senv) (report-unbound-var var)
          (if (null? (caar senv))
              (loop (cdr senv) (+ 1 depth) 0)
              (if (eqv? var (caaar senv))
                  (cons depth (cons pos (cdar senv)))
                  (loop (cons (cons (cdaar senv) (cdar senv)) (cdr senv)) depth (+ 1 pos))))))))

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
