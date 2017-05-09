(module tenv (lib "eopl.ss" "eopl")
        
        (require "drscheme-init.scm")
        (require "lang.scm")
        (require "data-structures.scm")

        (provide (all-defined-out))
        
        (define-datatype type-environment type-environment?
          (empty-tenv-record)
          (extended-tenv-record
           (sym symbol?)
           (type type?)
           (tenv type-environment?)))
        
        (define empty-tenv empty-tenv-record)
        (define extend-tenv extended-tenv-record)
        
        (define apply-tenv
          (lambda (tenv sym)
            (cases type-environment tenv
                   (empty-tenv-record ()
                                      (eopl:error 'apply-tenv "Unbound variable ~s" sym))
                   (extended-tenv-record (sym1 val1 old-env)
                                         (if (eqv? sym sym1)
                                             val1
                                           (apply-tenv old-env sym))))))
        
        (define init-tenv
          (lambda ()
            (extend-tenv 'x (int-type)
                         (extend-tenv 'v (int-type)
                                      (extend-tenv 'i (int-type)
                                                   (empty-tenv))))))
        
        ;; fresh-tvar-type : () -> Type
        ;; Page: 265
        (define fresh-tvar-type
          (let ((sn 0))
            (lambda ()
              (set! sn (+ sn 1))
              (tvar-type sn))))
        
        ;; otype->type : OptionalType -> Type
        ;; Page: 265
        (define otype->type
          (lambda (otype)
            (cases optional-type otype
                   (no-type () (fresh-tvar-type))
                   (a-type (ty) ty))))
        
        )
