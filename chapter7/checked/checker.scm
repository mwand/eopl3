(module checker (lib "eopl.ss" "eopl")

        (require "drscheme-init.scm")
        (require "lang.scm")

        (provide type-of type-of-program)

        ;; check-equal-type! : Type * Type * Exp -> Unspecified
        ;; Page: 242
        (define check-equal-type!
          (lambda (ty1 ty2 exp)
            (when (not (equal? ty1 ty2))
              (report-unequal-types ty1 ty2 exp))))

        ;; report-unequal-types : Type * Type * Exp -> Unspecified
        ;; Page: 243
        (define report-unequal-types
          (lambda (ty1 ty2 exp)
            (eopl:error 'check-equal-type!  
                        "Types didn't match: ~s != ~a in~%~a"
                        (type-to-external-form ty1)
                        (type-to-external-form ty2)
                        exp)))

        ;; find-duplicates : List -> Bool
        (define find-duplicates
          (lambda (lst)
            (cond ((null? lst) #f)
                  ((member (car lst) (cdr lst)) #t)
                  (else (find-duplicates (cdr lst))))))

        ;; report-unequal-types : List * Exp -> Unspecified
        (define report-let-duplicate-identifier
          (lambda (var-list exp)
            (eopl:error 'report-let-duplicate-identifier
                        "let: duplicate identifier: ~s in~%~a"
                        var-list
                        exp)))

        ;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

        ;; type-of-program : Program -> Type
        ;; Page: 244
        (define type-of-program
          (lambda (pgm)
            (cases program pgm
                   (a-program (exp1) (type-of exp1 (init-tenv))))))

        ;; type-of : Exp * Tenv -> Type
        ;; Page 244--246
        (define type-of
          (lambda (exp tenv)
            (cases expression exp

                   ;; \commentbox{\hastype{\tenv}{\mv{num}}{\mathtt{int}}}
                   (const-exp (num) (int-type))

                   ;; \commentbox{\hastype{\tenv}{\var{}}{\tenv{}(\var{})}}
                   (var-exp (var) (apply-tenv tenv var))

                   ;; \commentbox{\diffrule}
                   (diff-exp (exp1 exp2)
                             (let ((ty1 (type-of exp1 tenv))
                                   (ty2 (type-of exp2 tenv)))
                               (check-equal-type! ty1 (int-type) exp1)
                               (check-equal-type! ty2 (int-type) exp2)
                               (int-type)))

                   ;; \commentbox{\zerorule}
                   (zero?-exp (exp1)
                              (let ((ty1 (type-of exp1 tenv)))
                                (check-equal-type! ty1 (int-type) exp1)
                                (bool-type)))

                   ;; \commentbox{\condrule}
                   (if-exp (exp1 exp2 exp3)
                           (let ((ty1 (type-of exp1 tenv))
                                 (ty2 (type-of exp2 tenv))
                                 (ty3 (type-of exp3 tenv)))
                             (check-equal-type! ty1 (bool-type) exp1)
                             (check-equal-type! ty2 ty3 exp)
                             ty2))

                   ;; \commentbox{\letrule}
                   (let-exp (var-list exp-list body)
                            (if (find-duplicates var-list)
                              (report-let-duplicate-identifier var-list exp)
                              (let ((type-list (map (lambda (exp) (type-of exp tenv)) exp-list)))
                                    (type-of body
                                             (extend-list-tenv var-list type-list tenv)))))

                   ;; \commentbox{\procrulechurch}
                   (proc-exp (var var-type body)
                             (let ((result-type
                                     (type-of body
                                              (extend-tenv var var-type tenv))))
                               (proc-type var-type result-type)))

                   ;; \commentbox{\apprule}
                   (call-exp (rator rand) 
                             (let ((rator-type (type-of rator tenv))
                                   (rand-type  (type-of rand tenv)))
                               (cases type rator-type
                                      (proc-type (arg-type result-type)
                                                 (begin
                                                   (check-equal-type! arg-type rand-type rand)
                                                   result-type))
                                      (else
                                        (report-rator-not-a-proc-type rator-type rator)))))

                   ;; \commentbox{\letrecrule}
                   (letrec-exp (p-result-type-list p-name-list b-var-list b-var-type-list p-body-list
                                              letrec-body)
                               (let* ((letrec-statement-list 
                                       (zip p-result-type-list p-name-list b-var-list b-var-type-list p-body-list))
                                      (type-list (map proc-type b-var-type-list p-result-type-list))
                                      (tenv-for-letrec-body
                                       (extend-list-tenv p-name-list type-list tenv)))
                                 (let ((p-body-type-list
                                         (map (lambda (p-body) 
                                                (type-of p-body 
                                                         (extend-list-tenv b-var-list 
                                                                           b-var-type-list 
                                                                           tenv-for-letrec-body))) 
                                              p-body-list)))
                                   (for-each check-equal-type!
                                     p-body-type-list p-result-type-list p-body-list)
                                   (type-of letrec-body tenv-for-letrec-body)))))))

        (define report-rator-not-a-proc-type
          (lambda (rator-type rator)
            (eopl:error 'type-of-expression
                        "Rator not a proc type:~%~s~%had rator type ~s"   
                        rator 
                        (type-to-external-form rator-type))))

        ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

        (define-datatype type-environment type-environment?
                         (empty-tenv-record)
                         (extended-tenv-record
                           (sym symbol?)
                           (type type?)
                           (tenv type-environment?))
                         (extended-list-tenv-record
                           (sym-list (list-of symbol?))
                           (type-list (list-of type?))
                           (tenv type-environment?))
                         )

        (define empty-tenv empty-tenv-record)
        (define extend-tenv extended-tenv-record)
        (define extend-list-tenv extended-list-tenv-record)

        (define (zip . xss) (apply map list xss))

        (define apply-tenv 
          (lambda (tenv sym)
            (cases type-environment tenv
                   (empty-tenv-record ()
                                      (eopl:error 'apply-tenv "Unbound variable ~s" sym))
                   (extended-tenv-record (sym1 val1 old-env)
                                         (if (eqv? sym sym1) 
                                           val1
                                           (apply-tenv old-env sym)))
                   (extended-list-tenv-record (var-list type-list old-env)
                                            (let* ((zip-list (zip var-list type-list))
                                                   (result (assoc sym zip-list)))
                                              (if result 
                                                (cadr result)
                                                (apply-tenv old-env sym)))))))

        (define init-tenv
          (lambda ()
            (extend-tenv 'x (int-type) 
                         (extend-tenv 'v (int-type)
                                      (extend-tenv 'i (int-type)
                                                   (empty-tenv))))))

        )
