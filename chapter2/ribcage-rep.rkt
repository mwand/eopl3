#lang eopl

(require rackunit)

(define empty-env (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (list (list var) (list val)) env)))

(define extend-env*
  (lambda (vars vals env)
    (cons (list vars vals) env)))

(define apply-env
  (lambda (env search-var)
    (cond [(empty-env? env)
           (report-no-binding-found search-var)]
          [(eqv? (caaar env) search-var)
           (caadar env)]
          [(null? (cdaar env))
           (apply-env (cdr env) search-var)]
          [else
           (if (eqv? (caaar env) search-var)
               (caadar env)
               (apply-env (cons
                           (cons (cdaar env) (list (cdadar env)))
                           (cdr env))
                          search-var))
           ]
          )))

(define empty-env?
  (lambda (env)
    (null? env)))

(define has-binding?
  (lambda (s env)
    (cond
      [(empty-env? env) #f]
      [(eqv? (caaar env) s) #t]
      [(null? (cdaar env))
       (has-binding? s (cdr env))]
      [else
       (has-binding? s (cons
                        (cons (cdaar env) (list (cdadar env)))
                        (cdr env)))])))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))
(define e1
  (extend-env* '(x y) '(11 17)
               (extend-env* '(a b c) '(1 2 3) (empty-env))))

(check-equal? (apply-env e 'd) 6)
(check-equal? (apply-env e 'y) 8)
(check-equal? (apply-env e 'x) 7)
(check-false (has-binding? 'z e))
(check-true (has-binding? 'x e))

(check-equal? (apply-env e1 'a) 1)
(check-true (has-binding? 'x e1))
(check-true (has-binding? 'a e1))
(check-false (has-binding? 'z e1))

;; (report-unit-tests-completed 'apply-env)
