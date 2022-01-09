#lang eopl

(require "utils.rkt")
(provide (all-defined-out))
(define identifier?
  (lambda (sym)
    (cond
      [(eqv? sym 'lambda) #f]
      [else
       (symbol? sym)])))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;; occurs-free? : Sym * Lcexp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
           (var-exp (var) (eqv? var search-var))
           (lambda-exp (bound-var body)
                       (and
                        (not (eqv? search-var bound-var))
                        (occurs-free? search-var body)))
           (app-exp (rator rand)
                    (or
                     (occurs-free? search-var rator)
                     (occurs-free? search-var rand))))))

;; test items
(equal?? (occurs-free? 'x (var-exp 'x)) #t)

(equal?? (occurs-free? 'x (var-exp 'y)) #f)

(equal?? (occurs-free? 'x (lambda-exp 'x
                                      (app-exp (var-exp 'x) (var-exp 'y))))
         #f)

(equal??
 (occurs-free? 'x (lambda-exp 'y
                              (app-exp (var-exp 'x) (var-exp 'y))))
 #t)

(equal??
 (occurs-free? 'x
               (app-exp
                (lambda-exp 'x (var-exp 'x))
                (app-exp (var-exp 'x) (var-exp 'y))))
 #t)

(equal??
 (occurs-free? 'x
               (lambda-exp 'y
                           (lambda-exp 'z
                                       (app-exp (var-exp 'x)
                                                (app-exp (var-exp 'y) (var-exp 'z))))))
 #t)

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))

;; page 48: alternate definition
(define-datatype s-list-alt s-list-alt?
  (an-s-list
   (sexps (list-of s-exp?))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(eopl:printf "unit tests completed successfully.~%")

