#lang racket

(require rackunit)
(require eopl)

;; data-definition:
;; Env :: (empty-env)|(extendEnv Var Schemeval Env)

(define-datatype env env?
  (empty-env)
  (extend-env
   (var symbol?)
   (val number?)
   (old-env env?)))


(define (apply-env exp search-var)
  (cases env exp
         (empty-env () report-no-binding-found)
         (extend-env (var val old-env)
                     (if (equal? var search-var)
                         val
                         (apply-env old-env search-var)))))

(define (has-binding exp search-var)
  (cases env exp
         (empty-env () #f)
         (extend-env (var val old-env)
                     (if (equal? var search-var)
                         #t
                         (has-binding old-env search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for ~s" search-var)))


(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))

(module+ test
  (check-not-exn
   (lambda ()
             (apply-env (empty-env) 'a)))
  (check-exn exn:fail?
             (lambda () (error "here it is")))
  (check-eqv? (apply-env e 'd) 6)
  (check-eqv? (apply-env e 'x) 7)
  (check-false (has-binding (empty-env) 'a))
  (check-true (has-binding e 'y))
  (check-false (has-binding e 'a)))
