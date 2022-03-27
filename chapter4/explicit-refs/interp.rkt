#lang eopl

;; interpreter for the EXPLICIT-REFS language

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")
(require "store.rkt")

(provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

(define instrument-let (make-parameter #f))

;; say (instrument-let #t) to turn instrumentation on.
;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 110
(define value-of-program
  (lambda (pgm)
    ;; (initialize-store!)               ; new for explicit refs.
    (cases program pgm
           (a-program (exp1)
                      (cases answer (value-of exp1 (init-env) (init-store))
                             (an-answer (val store)
                                        val))))))

;; value-of : Exp * Env * Sto -> Answer
;; Page: 113
(define value-of
  (lambda (exp env sto)
    (letrec
        ([value-of*
          (lambda (exps store)
            (if (null? exps)
                (cons '() store)
                (cases answer (value-of (car exps) env store)
                       (an-answer (val new-store)
                                  (let
                                      ([vals1 (value-of* (cdr exps) new-store)])
                                      (cons (cons val (car vals1))
                                            (cdr vals1)))))))])
      (cases expression exp

             ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
             (const-exp (num)
                        (an-answer (num-val num) sto))

             ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
             (var-exp (var)
                      (an-answer
                       ;; (apply-store sto (apply-env env var))
                       (apply-env env var)
                       sto))

             ;\commentbox{\diffspec}
             (diff-exp (exp1 exp2)
                       (cases answer (value-of exp1 env sto)
                              (an-answer (val1 new-store1)
                                         (cases answer (value-of exp2 env new-store1)
                                                (an-answer (val2 new-store2)
                                                           (let ([num1 (expval->num val1)]
                                                                 [num2 (expval->num val2)])
                                                             (an-answer
                                                              (num-val (- num1 num2))
                                                              new-store2)))))))

             ;\commentbox{\zerotestspec}
             (zero?-exp (exp1)
                        (cases answer (value-of exp1 env sto)
                               (an-answer (val1 new-store)
                                          (let ((num1 (expval->num val1)))
                                            (if (zero? num1)
                                                (an-answer (bool-val #t) new-store)
                                                (an-answer (bool-val #f) new-store))))))

             ;\commentbox{\ma{\theifspec}}
             (if-exp (exp1 exp2 exp3)
                     (cases answer (value-of exp1 env sto)
                            (an-answer (val new-store)
                                       (if (expval->bool val)
                                           (value-of exp2 env new-store)
                                           (value-of exp3 env new-store)))))

             ;\commentbox{\ma{\theletspecsplit}}
             (let-exp (vars exps body)
                      (let ([vals-new-store (value-of* exps sto)])
                        (value-of body
                                  (extend-env* vars (car vals-new-store) env)
                                  (cdr vals-new-store))))

             (proc-exp (vars body)
                       (an-answer
                        (proc-val (procedure vars body env))
                        sto))

             (call-exp (rator rands)
                       (cases answer (value-of rator env sto)
                              (an-answer (proc new-store1)
                                         (let ([vals-new-store (value-of* rands new-store1)])
                                           (begin (display vals-new-store)
                                                  (newline)
                                                  (display "++++")
                                                  (newline)
                                                  (display proc)
                                                  (newline))
                                           (apply-procedure (expval->proc proc)
                                                            (car vals-new-store)
                                                            (cdr vals-new-store))))))
                                         ;; (cases answer (value-of rands env new-store1)
                                         ;;        (an-answer (arg new-store2)
                                         ;;                   (apply-procedure (expval->proc proc) arg new-store2))))))

             (letrec-exp (p-names b-vars p-bodies letrec-body)
                         (value-of letrec-body
                                   (extend-env-rec* p-names b-vars p-bodies env)
                                   sto))

             (begin-exp (exp1 exps)
                        (letrec
                            ([value-of-begins
                              (lambda (e1 es sto)
                                (let ([v1 (value-of e1 env sto)])
                                  (cases answer v1
                                         (an-answer (val new-store)
                                                    (if (null? es)
                                                        v1
                                                        (value-of-begins (car es) (cdr es) new-store))))))])
                          (value-of-begins exp1 exps sto)))

             (list-exp (exps)
                       (letrec
                           ([value-of-lst
                             (lambda (lst store)
                               (cond
                                 [(null? lst) (cons '() store)]
                                 [(null? (cdr lst))
                                  (cases answer (value-of (car lst) env store)
                                         (an-answer (head new-store)
                                                    (cons (list head) new-store)))]
                                 [else
                                  (cases answer (value-of (car lst) env store)
                                         (an-answer (head new-store)
                                                    (cons
                                                     (cons head (car (value-of-lst (cdr lst) new-store)))
                                                     (cdr (value-of-lst (cdr lst) new-store)))))]))])
                         (an-answer
                          (list-val (car (value-of-lst exps sto)))
                          (cdr (value-of-lst exps sto)))))

             (newref-exp (exp1)
                         (cases answer (value-of exp1 env sto)
                                (an-answer (v1 new-store)
                                           (let ([ref (newref v1 new-store)])
                                             (an-answer (ref-val (car ref)) (cdr ref))))))

             (deref-exp (exp1)
                        (cases answer (value-of exp1 env sto)
                               (an-answer (v1 new-store)
                                          (let ([ref1 (expval->ref v1)])
                                            (an-answer (deref ref1 new-store) new-store)))))

             (setref-exp (exp1 exp2)
                         (cases answer (value-of exp1 env sto)
                                (an-answer (ref new-store1)
                                           (cases answer (value-of exp2 env new-store1)
                                                  (an-answer (v2 new-store2)
                                                             (begin
                                                               (setref! (expval->ref ref) v2 new-store2)
                                                               (an-answer
                                                                (num-val 23)
                                                                new-store2)))))))
             ))))

;; apply-procedure : Proc * ExpVal -> ExpVal
;;
;; uninstrumented version
;;   (define apply-procedure
;;    (lambda (proc1 arg)
;;      (cases proc proc1
;;        (procedure (bvar body saved-env)
;;          (value-of body (extend-env bvar arg saved-env))))))

;; instrumented version
(define apply-procedure
  (lambda (proc1 args sto)
    (cases proc proc1
           (procedure (var body saved-env)
                      (let ([r args])
                        (let ([new-env (extend-env* var r saved-env)])
                          (when (instrument-let)
                            (begin
                              (eopl:printf
                               "entering body of proc ~s with env =~%"
                               var)
                              (pretty-print (env->list new-env))
                              (eopl:printf "store =~%")
                              (pretty-print (store->readable (get-store-as-list)))
                              (eopl:printf "~%")))
                          (value-of body new-env sto)))))))


;; store->readable : Listof(List(Ref,Expval))
;;                    -> Listof(List(Ref,Something-Readable))
(define store->readable
  (lambda (l)
    (map
     (lambda (p)
       (cons
        (car p)
        (expval->printable (cadr p))))
     l)))

