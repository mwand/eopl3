#lang eopl

(require (only-in racket
                  filter
                  flatten))

(require "lang.rkt")

;; (provide lookup-vars occurs-free?? difference lookup-free-vars)
(provide lookup-free-vars)


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

;; (define difference
;;   (lambda (lst1 lst2)
;;     (filter (lambda (v) (not (member v lst2))) lst1)))

