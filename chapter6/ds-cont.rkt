#lang eopl

(require racket)
(require rackunit)
(require racket/trace)

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

(define remove-fst
  (lambda (sym lst)
    (remove-fst/k sym lst (end-cont))))

(define remove-fst/k
  (lambda (sym lst cont)
    (if (null? lst)
        (apply-cont cont '())
        (if (eqv? (car lst) sym)
            (apply-cont cont (cdr lst))
            (remove-fst/k sym (cdr lst)
                          (remove-fst1-cont (car lst) cont))))))

;; list-sum : Listof(Int) -> Int
;; Page: 24
;; (define list-sum
;;   (lambda (loi)
;;     (if (null? loi)
;;         0
;;         (+ (car loi)
;;            (list-sum (cdr loi))))))

;; (equal?? (list-sum (list 1 2 3 4 5)) 15)
(define list-sum
  (lambda (lst)
    (list-sum/k lst (end-cont))))

(define list-sum/k
  (lambda (lst cont)
    (if (null? lst)
        (apply-cont cont 0)
        (list-sum/k (cdr lst)
                    (list-sum1-cont (car lst) cont)))))

;; occurs-free? : Sym * Lcexp -> Bool
;; usage:
;;   returns #t if the symbol var occurs free in exp,
;;   otherwise returns #f.
;; Page: 19
;; (define occurs-free?
;;   (lambda (var exp)
;;     (cond
;;       ((symbol? exp) (eqv? var exp))
;;       ((eqv? (car exp) 'lambda)
;;        (and
;;         (not (eqv? var (car (cadr exp))))
;;         (occurs-free? var (caddr exp))))
;;       (else
;;        (or
;;         (occurs-free? var (car exp))
;;         (occurs-free? var (cadr exp)))))))
(define occurs-free?
  (lambda (var exp)
    (occurs-free/k var exp (end-cont))))

(define occurs-free/k
  (lambda (var exp cont)
    (cond
      [(symbol? exp) (apply-cont cont (eqv? var exp))]
      [(eqv? (car exp) 'lambda)
       (occurs-free/k var (caddr exp)
                      (occurs-free1-cont
                       (not (eqv? var (car (cadr exp))))
                       cont))]
      [else
       (occurs-free/k var (car exp)
                      (occurs-free2-cont var (cadr exp) cont))]
      )))

;; subst : Sym * Sym * S-list -> S-list
;; Page: 21
;; (define subst
;;   (lambda (new old slist)
;;     (if (null? slist)
;;         '()
;;         (cons
;;          (subst-in-s-exp new old (car slist))
;;          (subst new old (cdr slist))))))
(define subst
  (lambda (sym1 sym2 lst)
    (subst/k sym1 sym2 lst (end-cont))))

(define subst/k
  (lambda (sym1 sym2 lst cont)
    (if (null? lst)
        (apply-cont cont '())
        (subst-in-s-exp/k sym1 sym2 (car lst)
                          (subst-cont sym1 sym2 (cdr lst) cont)))))

(define subst-in-s-exp/k
  (lambda (new old s-exp cont)
    (if (symbol? s-exp)
        (if (equal? old s-exp)
            (apply-cont cont new)
            (apply-cont cont s-exp))
        (subst/k new old s-exp cont))))

(define-datatype continuation continuation?
  (end-cont)
  (remove-fst1-cont
   (head symbol?)
   (saved-cont continuation?))
  (list-sum1-cont
   (head number?)
   (saved-cont continuation?))
  (occurs-free1-cont
   (rest boolean?)
   (saved-cont continuation?))
  (occurs-free2-cont
   ;; (exp1 lc-exp?)
   (var1 symbol?)
   (exp1 (lambda (e)
           (or (symbol? e)
               (eqv? (car e) 'lambda)
               (pair? e))))
   (saved-cont continuation?))
  (occurs-free3-cont
   (rest boolean?)
   (saved-cont continuation?))
  (subst-cont
   (sym1 symbol?)
   (sym2 symbol?)
   (lst1 list?)
   (saved-cont continuation?))
  (subst1-cont
   (old (lambda (e)
          (or (symbol? e)
              (list? e))))
   (saved-cont continuation?))
  )

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont ()
                     (begin
                       (eopl:printf "End of computation.~%")
                       (eopl:printf "This sentence should appear only once.~%")
                       val))
           (remove-fst1-cont (head cont)
                             (apply-cont cont (cons head val)))
           (list-sum1-cont (head cont)
                           (apply-cont cont (+ head val)))
           (occurs-free1-cont (b cont)
                              (apply-cont cont (and b val)))
           (occurs-free2-cont (var1 exp1 cont)
                              (occurs-free/k var1 exp1 (occurs-free3-cont val cont)))
           (occurs-free3-cont (b cont)
                              (apply-cont cont (or b val)))
           (subst-cont (sym1 sym2 lst1 cont)
                       (subst/k sym1 sym2 lst1 (subst1-cont val cont)))
           (subst1-cont (val1 cont)
                        (apply-cont cont (cons val1 val)))
           )))

(module+ test
  (check-equal? (remove-first 'a '(a b c)) '(b c))
  (check-equal? (remove-first 'b '(e f g)) '(e f g))
  (check-equal? (remove-first 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
  (check-equal? (remove-first 'x '()) '())
  (check-equal? (remove-fst 'a '(a b c)) '(b c))
  (check-equal? (remove-fst 'b '(e f g)) '(e f g))
  (check-equal? (remove-fst 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
  (check-equal? (remove-fst 'x '()) '())
  (check-equal? (list-sum '(1 2 3 4 5)) 15)
  (check-equal? (list-sum '(1 2 3 4)) 10)
  (check-equal? (list-sum '()) 0)
  (check-equal? (occurs-free? 'x 'x) #t)
  (check-equal? (occurs-free? 'x 'y) #f)
  (check-equal? (occurs-free? 'x '(lambda (x) (x y))) #f)
  (check-equal? (occurs-free? 'x '(lambda (y) (x y))) #t)
  (check-equal? (occurs-free? 'x '((lambda (x) x) (x y))) #t)
  (check-equal? (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))) #t)
  (check-equal? (subst 'b 'a '(a c () (i (a) a))) '(b c () (i (b) b)))
  )

;; (trace remove-fst remove-fst/k apply-cont)
;; (trace apply-cont subst/k subst-in-s-exp/k)
;; (define lambda-exp?
;;   (λ (E)
;;     (letrec
;;         ([p
;;           (λ (e)
;;             (match e
;;               [`,y #t]
;;               [`(lambda (,x) ,body) (p body)]
;;               [`(,rator ,rand . ,more) (or (p rator) (p rand))]
;;               [else #f]))])
;;       (p E))))

