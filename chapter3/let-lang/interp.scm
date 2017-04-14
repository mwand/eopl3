(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require scheme/base)
  (require "drscheme-init.scm")
  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (num-val 1)
                (num-val 0)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (not (equal? (expval->num val1) 0))
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))

        (minus-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (num-val (- 0 (expval->num val1)))))

        (addition-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val2)))
                          (num-val
                            (+ num1 num2)))))

        (multiplication-exp (exp1 exp2)
                            (let ((val1 (value-of exp1 env))
                                  (val2 (value-of exp2 env)))
                              (let ((num1 (expval->num val1))
                                    (num2 (expval->num val2)))
                                (num-val
                                  (* num1 num2)))))

        (quotient-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val2)))
                          (num-val
                            (/ num1 num2)))))

        (equal?-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (if (equal? num1 num2)
                          (num-val 1)
                          (num-val 0)))))

        (greater?-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (if (> num1 num2)
                          (num-val 1)
                          (num-val 0)))))

        (less?-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (if (< num1 num2)
                          (num-val 1)
                          (num-val 0)))))

        (cons-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (cons-val val1 val2)))

        (emptylist-exp ()
                       (emptylist-val))

        (null?-exp (exp)
                   (let ((val (value-of exp env)))
                     (expval->null? val)))

        (car-exp (exp)
                 (let ((val (value-of exp env)))
                   (expval->car val)))

        (cdr-exp (exp)
                 (let ((val (value-of exp env)))
                   (expval->cdr val)))

        (list-exp (args)
                  (list-val (map (lambda (arg)
                                   (value-of arg env))
                                 args)))

        (cond-exp (predicates values)
                  (define inner
                    (lambda (predicates values)
                      (if (or (null? predicates) (null? values))
                        (eopl:error 'cond-exp "This cond expression have no value")
                        (let ((predicate (value-of (car predicates) env))
                              (value (value-of (car values) env)))
                          (let ((predicate-num (expval->num predicate))
                                (num (expval->num value)))
                            (if (not (equal? predicate-num 0))
                              (num-val num)
                              (inner (cdr predicates) (cdr values))))))))
                  (inner predicates values))

        )))


  )

