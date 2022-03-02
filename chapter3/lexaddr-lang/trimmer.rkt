#lang eopl

(require (only-in racket
                  filter
                  flatten
                  andmap
                  ormap))

(require "lang.rkt")

;; (provide lookup-vars occurs-free?? difference lookup-free-vars)
(provide lookup-free-vars can-inline? proc-exp? inline-of inline-of* difference)


;;;;;;;;;;;;;;;; trimmed representation ;;;;;;;;;;;;;;
(define lookup-free-vars
  (lambda (syms exp)
    (let ([lookup-curry (lambda (e) (lookup-free-vars syms e))])
      (cases expression exp
             (diff-exp (exp1 exp2)
                       (flatten
                        (append (lookup-free-vars syms exp1)
                                (lookup-free-vars syms exp2))))
             (zero?-exp (exp1)
                        (lookup-free-vars syms exp1))
             (if-exp (exp1 exp2 exp3)
                     (flatten
                      (append
                       (lookup-free-vars syms exp1)
                       (lookup-free-vars syms exp2)
                       (lookup-free-vars syms exp3))))
             (var-exp (var)
                      (if (not (member var syms))
                          (list var)
                          '()))
             (let-exp (vars exps body)
                      (lookup-free-vars (append syms vars) body))
             (proc-exp (vars body)
                       (lookup-free-vars (append syms vars) body))
             (call-exp (rator rands)
                       (flatten
                        (append
                         (lookup-free-vars syms rator)
                         (map lookup-curry rands))))
             (cond-exp (exps1 exps2)
                       (flatten
                        (append
                         (map lookup-curry exps1)
                         (map lookup-curry exps2))))
             (cons-exp (head tail)
                       (flatten
                        (append
                         (lookup-free-vars syms head)
                         (lookup-free-vars syms tail))))
             (car-exp (exp)
                      (lookup-free-vars syms exp))
             (cdr-exp (exp)
                      (lookup-free-vars syms exp))
             (list-exp (exps)
                       (flatten
                        (map lookup-curry exps)))
             (unpack-exp (vars lst body)
                         (lookup-free-vars (append syms vars) body))
             (letrec-exp (p-names b-varss p-bodies letrec-body)
                         (lookup-free-vars (append syms p-names) letrec-body))
             (else '())
             ))))


;;;;;;;;;;;;;; inline proc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define is-proc?
;;   (lambda (search-var exp)
;;     (cases expression exp
;;            ())))
(define proc-exp?
  (lambda (exp)
    (cases expression exp
           (proc-exp (vars body)
                     #t)
           (else #f))))

(define var-exp?
  (lambda (exp)
    (cases expression exp
           (var-exp (var)
                    #t)
           (else #f))))

(define call-exp?
  (lambda (exp)
    (cases expression exp
           (call-exp (rator rands)
                     #t)
           (else #f))))

(define let-exp?
  (lambda (exp)
    (cases expression exp
           (let-exp (vars exps body)
                    #t)
           (else #f))))

(define can-inline?
  (lambda (search-var exp)
    (let ([can-inline-curry (lambda (e) (can-inline? search-var e))])
      (cases expression exp
             (diff-exp (exp1 exp2)
                       (and (can-inline? search-var exp1)
                            (can-inline? search-var exp2)))
             (zero?-exp (exp1)
                        (can-inline? search-var exp1))
             (if-exp (exp1 exp2 exp3)
                     (and
                      (can-inline? search-var exp1)
                      (can-inline? search-var exp2)
                      (can-inline? search-var exp3)))
             (let-exp (vars exps body)
                      (and
                       (andmap can-inline-curry exps)
                       (can-inline? search-var body)))
             (proc-exp (vars body)
                       (can-inline? search-var body))
             (call-exp (rator rands)
                       (and
                        (or
                         (not (member (var-exp search-var) (filter var-exp? rands)))
                         (and (not (equal? rator (var-exp search-var)))
                              (member (var-exp search-var) (filter var-exp? rands))))
                        (can-inline? search-var rator)
                        (andmap can-inline-curry (filter (lambda (e) (not (var-exp? e))) rands))))
             (cond-exp (exps1 exps2)
                       (and (andmap can-inline-curry exps1)
                            (andmap can-inline-curry exps2)))
             (cons-exp (head tail)
                      (and (can-inline? search-var head)
                           (can-inline? search-var tail)))
             (car-exp (exp1)
                      (can-inline? search-var exp1))
             (cdr-exp (exp1)
                      (can-inline? search-var exp1))
             (list-exp (exps1)
                       (and (andmap can-inline-curry exps1)))
             (unpack-exp (syms lst body)
                         (and (andmap can-inline-curry lst)
                              (can-inline? search-var body)))
             (letrec-exp (p-names b-vars p-bodies letrec-body)
                         (and (andmap can-inline-curry p-bodies)
                              (can-inline? search-var letrec-body)))
             (else #t)))))

(define inline-of
  (lambda (sym replace exp)
    (let ([inline-of-curry (lambda (e) (inline-of sym replace e))])
    (cases expression exp
           (diff-exp (exp1 exp2)
                     (diff-exp
                      (inline-of-curry exp1)
                      (inline-of-curry exp2)))
           (zero?-exp (exp1)
                      (zero?-exp (inline-of-curry exp1)))
           (if-exp (exp1 exp2 exp3)
                   (if-exp
                    (inline-of-curry exp1)
                    (inline-of-curry exp2)
                    (inline-of-curry exp3)))
           (var-exp (var1)
                    (if (eqv? sym var1)
                        replace
                        (var-exp var1)))
           (let-exp (vars exps body)
                    (if (not (member sym vars))
                        (let-exp vars
                                 (map inline-of-curry exps)
                                 (inline-of-curry body))
                        (let-exp vars exps body)))
           (proc-exp (vars body)
                     (if (not (member sym vars))
                         (proc-exp vars (inline-of-curry body))
                         (proc-exp vars body)))
           (call-exp (rator rands)
                     (let* ([new-exp (call-exp
                                      (inline-of-curry rator)
                                      (map inline-of-curry rands))]
                            [real-exp
                             (if (ormap call-exp? rands)
                                 new-exp
                                 (cases expression replace
                                        (proc-exp (vars body)
                                                  (if (equal? (var-exp sym) rator)
                                                      (inline-of* vars rands body)
                                                      new-exp))
                                        (else new-exp)))])
                       real-exp))
           (cond-exp (exps1 exps2)
                     (cond-exp
                      (map inline-of-curry exps1)
                      (map inline-of-curry exps2)))
           (cons-exp (head tail)
                     (cons-exp (inline-of-curry head)
                               (inline-of-curry tail)))
           (car-exp (exp1)
                    (car-exp (inline-of-curry exp1)))
           (cdr-exp (exp1)
                    (cdr-exp (inline-of-curry exp1)))
           (list-exp (exps1)
                     (list-exp (inline-of-curry exps1)))
           (unpack-exp (vars lst body)
                       (if (not (member sym vars))
                           (unpack-exp vars
                                       (map inline-of-curry lst)
                                       (inline-of-curry body))
                           (unpack-exp vars lst body)))
           (letrec-exp (p-names b-vars p-bodies letrec-body)
                        (if (and (not (member sym p-names))
                                 (not (member sym b-vars)))
                            (letrec-exp p-names b-vars
                                        (map inline-of-curry p-bodies)
                                        (inline-of-curry letrec-body))
                            (letrec-exp p-names b-vars p-bodies letrec-body)))
           (else exp)
           ))))


(define inline-of*
  (lambda (vars exps body)
    (if (null? vars)
        body
        (inline-of* (cdr vars)
                    (cdr exps)
                    (inline-of (car vars) (car exps) body)))))

;; (define lookup-free-vars1
;;   (lambda (vars body)
;;     (filter
;;      (occurs-free?? body)
;;      (difference ;; vars
;;       (flatten (lookup-vars body))
;;       vars
;;       ))))

;; (define lookup-vars
;;   (lambda (exp)
;;     (cases expression exp
;;            ;; (const-exp (num) '())
;;            (diff-exp (exp1 exp2)
;;                      (append (lookup-vars exp1)
;;                              (lookup-vars exp2)))
;;            (zero?-exp (exp1)
;;                       (lookup-vars exp1))
;;            (if-exp (exp1 exp2 exp3)
;;                    (append (lookup-vars exp1)
;;                            (lookup-vars exp2)
;;                            (lookup-vars exp3)))
;;            (var-exp (var)
;;                     (list var))
;;            (let-exp (vars exps body)
;;                     (append
;;                      (map lookup-vars exps)
;;                      (lookup-vars body)))
;;            (proc-exp (vars body)
;;                      (lookup-vars body))
;;            (call-exp (rator rands)
;;                      (append (lookup-vars rator)
;;                              (map lookup-vars rands)))
;;            (cond-exp (exps1 exps2)
;;                      (append (map lookup-vars exps1)
;;                              (map lookup-vars exps2)))
;;            (cons-exp (head tail)
;;                      (append (lookup-vars head)
;;                              (lookup-vars tail)))
;;            (car-exp (exp)
;;                     (lookup-vars exp))
;;            (cdr-exp (exp)
;;                     (lookup-vars exp))
;;            (list-exp (exps)
;;                      (map lookup-vars exps))
;;            (unpack-exp (syms lst body)
;;                        (append (lookup-vars lst)
;;                                (lookup-vars body)))
;;            (letrec-exp (p-names b-vars p-bodies letrec-body)
;;                        (append
;;                         ;; p-names
;;                         ;; b-vars
;;                         (map lookup-vars p-bodies)
;;                         (lookup-vars letrec-body)))
;;            (else '())
;;            )))

;; ;; occurs-free? : Sym * Lcexp -> Bool
;; ;; if sym occur in lcexp true, else false
;; (define occurs-free??
;;   (lambda (exp)
;;     (lambda (search-var)
;;       (occurs-free? search-var exp))))

;; (define occurs-free?
;;   (lambda (search-var exp)
;;     (let ([occurs-free-curry (lambda (e) (occurs-free? search-var e))])
;;     (cases expression exp
;;            (const-exp (num) #f)

;;            (diff-exp (exp1 exp2)
;;                      (or
;;                       (occurs-free? search-var exp1)
;;                       (occurs-free? search-var exp2)))

;;            (zero?-exp (exp1)
;;                       (occurs-free? search-var exp1))

;;            (if-exp (exp1 exp2 exp3)
;;                    (or
;;                     (occurs-free? search-var exp1)
;;                     (occurs-free? search-var exp2)
;;                     (occurs-free? search-var exp3)))

;;            (var-exp (var)
;;                     (eqv? var search-var))

;;            (let-exp (vars exps body)
;;                     (and
;;                      (not (member search-var vars))
;;                      (occurs-free? search-var body)))

;;            (proc-exp (vars body)
;;                      (and
;;                       (not (member search-var vars))
;;                       (occurs-free? search-var body)))

;;            (call-exp (rator rands)
;;                      (or (occurs-free? search-var rator)
;;                          (or-lst-bool
;;                           (map occurs-free-curry rands))))

;;            (cond-exp (exps1 exps2)
;;                      (or
;;                       (or-lst-bool
;;                        (map occurs-free-curry exps1))
;;                       (or-lst-bool
;;                        (map occurs-free-curry exps2))))

;;            (emptylist-exp () #f)

;;            (cons-exp (head tail)
;;                      (or
;;                       (occurs-free? search-var head)
;;                       (occurs-free? search-var tail)))

;;            (car-exp (exp)
;;                     (occurs-free? search-var exp))

;;            (cdr-exp (exp)
;;                     (occurs-free? search-var exp))

;;            (list-exp (exps)
;;                      (or-lst-bool
;;                       (map occurs-free-curry exps)))

;;            (unpack-exp (syms lst body)
;;                        (and
;;                         (not (member search-var syms))
;;                         (or
;;                          (occurs-free? search-var body)
;;                          (occurs-free? search-var lst))))

;;            (letrec-exp (p-names b-vars p-bodies letrec-body)
;;                        (and
;;                         (not (member search-var p-names))
;;                         (not (member search-var (flatten b-vars)))
;;                         (or
;;                          (or-lst-bool
;;                           (map occurs-free-curry p-bodies))
;;                          (occurs-free? search-var letrec-body))))

;;            (else #f)))))

;; ;; helper func for occur-free?
;; (define or-lst-bool
;;   (lambda (lst)
;;     (or-lst-iter lst #f)))

;; (define or-lst-iter
;;   (lambda (lst initial)
;;     (if (null? lst)
;;         initial
;;         (or-lst-iter (cdr lst) (or (car lst) initial)))))

(define difference
  (lambda (lst1 lst2)
    (filter (lambda (v) (not (member v lst2))) lst1)))

(define get-program-body
  (lambda (pgm)
    (cases program (scan&parse pgm)
           (a-program (exp1) exp1))))
