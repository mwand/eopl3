#lang racket

(require eopl)
(require "utils.rkt")

;; data definitions
(define identifier? symbol?)

(define-datatype lc-exp lc-exp? 
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;; parse-expression : Schemeval -> Lcexp
;; page 53
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (report-invalid-concrete-syntax datum)))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error "invalid concrete syntax ~s" datum)))

;; unit tests
(equal??
 (parse-expression 'x)
 (var-exp 'x))

(equal??
 (parse-expression 'y)
 (var-exp 'y))

(equal??
 (parse-expression '(lambda (x) (x y)))
 (lambda-exp 'x
             (app-exp (var-exp 'x) (var-exp 'y))))

(equal??
 (parse-expression '(lambda (y) (x y)))
 (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y))))

(equal??
 (parse-expression '((lambda (x) x) (x y))) 
 (app-exp
  (lambda-exp 'x (var-exp 'x))
  (app-exp (var-exp 'x) (var-exp 'y))))

(equal?? 
 (parse-expression '(lambda (y) (lambda (z) (x (y z)))))
 (lambda-exp 'y
             (lambda-exp 'z
                         (app-exp (var-exp 'x)
                                  (app-exp (var-exp 'y) (var-exp 'z))))))

(report-unit-tests-completed 'parse-expression)

;; unparse-lc-exp : Lcexp -> Schemeval
;; page 53
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var) var)
           (lambda-exp (bound-var body) 
                       (list 'lambda (list bound-var)
                             (unparse-lc-exp body)))
           (app-exp (rator rand)
                    (list 
                     (unparse-lc-exp rator) (unparse-lc-exp rand))))))


;; unit tests
(equal??
 (unparse-lc-exp (var-exp 'x))
 'x)

(equal??
 (unparse-lc-exp (var-exp 'y))
 'y)

(equal??
 (unparse-lc-exp
  (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
 '(lambda (x) (x y)))

(equal??
 (unparse-lc-exp
  (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y))))
 '(lambda (y) (x y)))

(equal??
 (unparse-lc-exp
  (app-exp
   (lambda-exp 'x (var-exp 'x))
   (app-exp (var-exp 'x) (var-exp 'y))))
 '((lambda (x) x) (x y)))


(equal??
 (unparse-lc-exp
  (lambda-exp 'y
              (lambda-exp 'z
                          (app-exp (var-exp 'x)
                                   (app-exp (var-exp 'y) (var-exp 'z))))))
 '(lambda (y) (lambda (z) (x (y z)))))

;; (equal??
;;  (parse-expression '(())))
(report-unit-tests-completed 'unparse-lc-exp)


;; Exercise 2.27
;;   ((lambda (a) (a b)) c)

;;   (lambda (x)
;;     (lambda (y)
;;       ((lambda (x)
;;          (x y))
;;        x)))

;; ex 2.28
(define (unparse exp)
  (cases lc-exp exp
         (var-exp (var) (symbol->string var))
         (lambda-exp (bound-var body)
                     (format "(lambda (~a) ~a)" bound-var (unparse body)))
         (app-exp (rator rand)
                  (format "(~a ~a)" (unparse rator) (unparse rand)))))

