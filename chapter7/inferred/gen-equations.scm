(module gen-equations (lib "eopl.ss" "eopl")
        
        (require "drscheme-init.scm")
        (require "lang.scm")
        (require "data-structures.scm")
        (require "unifier.scm")
        (require "substitutions.scm")
        (require racket/base)
        (require "tenv.scm")
        
        (provide gen-equations)
        
        ;; gen-equations: Exp -> Type * Equations
        (define gen-equations
          (lambda (exp tenv)

            (cases expression exp
                   
                   (const-exp (num)
                              (cons (int-type) '()))
                   
                   (zero?-exp (exp1)
                              (let* ((pair (gen-equations exp1 tenv))
                                     (tvar-exp1 (car pair))
                                     (tvar-zero?-exp (fresh-tvar-type))
                                     (equations (cdr pair))
                                     (new-equations (list* (cons tvar-exp1 (int-type))
                                                           (cons tvar-zero?-exp (bool-type))
                                                           equations)))
                                (cons tvar-zero?-exp new-equations)))
                   
                   (diff-exp (exp1 exp2)
                             (let* ((pair1 (gen-equations exp1 tenv))
                                    (pair2 (gen-equations exp2 tenv))
                                    (tvar-exp1 (car pair1))
                                    (tvar-exp2 (car pair2))
                                    (tvar-diff-exp (fresh-tvar-type))
                                    (equations-exp1 (cdr pair1))
                                    (equations-exp2 (cdr pair2))
                                    (new-equations (list* (cons tvar-exp1 (int-type))
                                                          (cons tvar-exp2 (int-type))
                                                          (cons tvar-diff-exp (int-type))
                                                          (append equations-exp1 equations-exp2))))
                               (cons tvar-diff-exp new-equations)))
                   
                   (if-exp (exp1 exp2 exp3)
                           (let* ((pair1 (gen-equations exp1 tenv))
                                  (pair2 (gen-equations exp2 tenv))
                                  (pair3 (gen-equations exp3 tenv))
                                  (tvar-exp1 (car pair1))
                                  (tvar-exp2 (car pair2))
                                  (tvar-exp3 (car pair3))
                                  (tvar-if-exp (fresh-tvar-type))
                                  (equations-exp1 (cdr pair1))
                                  (equations-exp2 (cdr pair2))
                                  (equations-exp3 (cdr pair3))
                                  (new-equations (list* (cons tvar-exp1 (bool-type))
                                                        (cons tvar-exp2 tvar-exp3)
                                                        (cons tvar-if-exp tvar-exp2)
                                                        (append equations-exp1
                                                                equations-exp2
                                                                equations-exp3))))
                             (cons tvar-if-exp new-equations)))

                   (var-exp (var) (cons (apply-tenv tenv var) '()))

                   (let-exp (var exp1 body)
                            (let* ((tvar-var (fresh-tvar-type))
                                   (pair-exp1 (gen-equations exp1 tenv))
                                   (pair-body (gen-equations body (extend-tenv var tvar-var tenv)))
                                   (tvar-exp1 (car pair-exp1))
                                   (tvar-body (car pair-body))
                                   (equations-exp1 (cdr pair-exp1))
                                   (equations-body (cdr pair-body))
                                   (tvar-let-exp (fresh-tvar-type))
                                   (new-equations (list* (cons tvar-var tvar-exp1)
                                                         (cons tvar-let-exp tvar-body)
                                                         (append equations-exp1
                                                                 equations-body))))
                              (cons tvar-let-exp new-equations)))

                   (proc-exp (var otype body)
                             (let* ((arg-type (otype->type otype))
                                    (pair-body (gen-equations body (extend-tenv var arg-type tenv)))
                                    (tvar-body (car pair-body))
                                    (equations-body (cdr pair-body))
                                    (tvar-proc-exp (fresh-tvar-type))
                                    (new-equations (list* (cons tvar-proc-exp 
                                                                (proc-type arg-type tvar-body))
                                                          equations-body)))
                               (cons tvar-proc-exp new-equations)))

                   (call-exp (rator rand)
                             (let* ((result-type (fresh-tvar-type))
                                    (pair-rator (gen-equations rator tenv))
                                    (pair-rand (gen-equations rand tenv))
                                    (tvar-rator (car pair-rator))
                                    (equations-rator (cdr pair-rator))
                                    (tvar-rand (car pair-rand))
                                    (equations-rand (cdr pair-rand))
                                    (new-equations (list* (cons tvar-rator
                                                                (proc-type tvar-rand result-type))
                                                          (append equations-rator
                                                                  equations-rand))))
                               (cons result-type new-equations)))

                   (letrec-exp (proc-result-otype 
                                proc-name
                                bvar 
                                proc-arg-otype
                                proc-body
                                letrec-body)
                               (let* ((proc-result-type (otype->type proc-result-otype))
                                      (proc-arg-type (otype->type proc-arg-otype))
                                      (tenv-for-letrec-body (extend-tenv proc-name 
                                                                         (proc-type proc-arg-type proc-result-type)
                                                                         tenv))
                                      (pair-proc-body (gen-equations proc-body (extend-tenv bvar proc-arg-type tenv-for-letrec-body)))
                                      (tvar-proc-body (car pair-proc-body))
                                      (equations-proc-body (cdr pair-proc-body))
                                      (pair-letrec-body (gen-equations letrec-body tenv-for-letrec-body))
                                      (tvar-letrec-body (car pair-letrec-body))
                                      (equations-letrec-body (cdr pair-letrec-body)))
                                 (cons tvar-letrec-body (list* (cons tvar-proc-body proc-result-type)
                                                               (append equations-proc-body
                                                                       equations-letrec-body)))))

                   )))
               
        )
