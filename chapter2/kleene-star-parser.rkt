#lang racket

;; ex 2.29
;| Lc-exp ::= Identifier
;             var-exp (var)
;         ::= (lambda ({Identifier}∗) Lc-exp)
;             lambda-exp (bound-vars body)
;         ::=(Lc-exp {Lc-exp}∗)
;             app-exp (rator rands)
;|

(require eopl)
(require rackunit)

(define (identifier? x)
  (and (symbol? x)
       (not (eqv? x 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-var (list-of identifier?))
              (body lc-exp?))
  (app-exp (rator lc-exp?)
           (rands (list-of lc-exp?))))

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (cadr datum)
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (map parse-expression (cdr datum)))))
      (else (report-invalid-concrete-syntax datum)))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (error "invalid concrete syntax ~s" datum)))


(module+ test
  (check-equal? (parse-expression 'x)
                (var-exp 'x))
  (check-equal? (parse-expression '(lambda (a b) (a b)))
                (lambda-exp '(a b) (var-exp 'a+b)))
  )
