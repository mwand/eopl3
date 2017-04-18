(module substitutions  (lib "eopl.ss" "eopl")
        (require "drscheme-init.scm")
        (require "lang.scm")
        (require "data-structures.scm")
        (require "../../utils.scm")
        
        (provide substitution? empty-subst extend-subst apply-subst-to-type)
        
        ;;;;;;;;;;;;;;;; Unit substitution ;;;;;;;;;;;;;;;;
        
        ;; apply-one-subst: type * tvar * type -> type
        ;; (apply-one-subst ty0 var ty1) returns the type obtained by
        ;; substituting ty1 for every occurrence of tvar in ty0.  This is
        ;; sometimes written ty0[tvar=ty1]
        
        ;; apply-one-subst : Type * Tvar * Type -> Type
        ;; Page: 260
        (define apply-one-subst
          (lambda (ty0 tvar ty1)
            (cases type ty0
                   (int-type () (int-type))
                   (bool-type () (bool-type))
                   (proc-type (arg-type result-type)
                              (proc-type
                               (apply-one-subst arg-type tvar ty1)
                               (apply-one-subst result-type tvar ty1)))
                   (tvar-type (sn)
                              (if (equal? ty0 tvar) ty1 ty0)))))
        
        ;;;;;;;;;;;;;;;; Substitutions ;;;;;;;;;;;;;;;;
        
        ;; a substitution is a map from unknown types to types.
        ;; we'll represent this as an association list.
        
        (define pair-of
          (lambda (pred1 pred2)
            (lambda (val)
              (and (pair? val) (pred1 (car val)) (pred2 (cdr val))))))
        
        (define substitution?
          (list-of (pair-of tvar-type? type?)))
        
        ;; basic observer: apply-subst-to-type
        ;; this is sometimes written ty1.subst
        
        ;; apply-subst-to-type : Type * Subst -> Type
        ;; Page: 261
        ;; (define apply-subst-to-type
        ;;   (lambda (ty subst)
        ;;     (cases type ty
        ;;       (int-type () (int-type))
        ;;       (bool-type () (bool-type))
        ;;       (proc-type (t1 t2)
        ;;         (proc-type
        ;;           (apply-subst-to-type t1 subst)
        ;;           (apply-subst-to-type t2 subst)))
        ;;       (tvar-type (sn)
        ;;         (let ((tmp (assoc ty subst)))
        ;;           (if tmp
        ;;             (cdr tmp)
        ;;             ty))))))
        
        ;; Ex7.17
        ;; Page: 262
        (define apply-subst-to-type
          (lambda (ty subst)
            (display subst)
            (cases type ty
                   (int-type () (int-type))
                   (bool-type () (bool-type))
                   (proc-type (t1 t2)
                              (proc-type
                               (apply-subst-to-type t1 subst)
                               (apply-subst-to-type t2 subst)))
                   (tvar-type (sn)
                              (let ((tmp (assoc ty subst)))
                                (if tmp
                                    ;; t(σ[tv = t']) = (tσ)[tv = t']
                                    (reduce (lambda (sub ty) 
                                              (apply-one-subst ty (car sub) (cdr sub))) 
                                            subst 
                                            (cdr tmp))
                                  ty))))))
        
        ;; empty-subst : () -> Subst
        ;; produces a representation of the empty substitution.
        
        ;; extend-subst : Subst * Tvar * Type -> Subst
        
        ;; (extend-subst s tv t) produces a substitution with the property
        ;; that for all t0,
        
        ;;   (apply-subst t0 (extend-subst s tv t))
        ;;   = (apply-one-subst (apply-subst t0 s) tv t)
        
        ;; i.e.,  t0.(s[tv=t]) = (t0.s)[tv=t]
        
        ;; this means that for any type variable tv0 in the domain of s,
        
        ;;   (apply-subst tv0 (extend-subst s tv t))
        ;;   = (apply-one-subst (apply-subst tv0 s) tv t)
        
        ;; so we extend the substitution with a new element, and apply [t/v] to every
        ;; element already in the substitution.
        
        
        ;; empty-subst : () -> Subst
        ;; Page 262
        (define empty-subst (lambda () '()))
        
        ;; extend-subst : Subst * Tvar * Type -> Subst
        ;; usage: tvar not already bound in subst.
        ;; Page: 262
        ;; (define extend-subst
        ;;   (lambda (subst tvar ty)
        ;;     (cons
        ;;       (cons tvar ty)
        ;;       (map
        ;;         (lambda (p)
        ;;           (let ((oldlhs (car p))
        ;;                 (oldrhs (cdr p)))
        ;;             (cons
        ;;               oldlhs
        ;;               (apply-one-subst oldrhs tvar ty))))
        ;;         subst))))
        
        ;; Ex7.17
        ;; Page: 262
        ;; The extra work is shifted to apply-subst-to-type, so that the property
        ;; `t(σ[tv = t']) = (tσ)[tv = t']` is still satisﬁed.
        (define extend-subst
          (lambda (subst tvar ty)
            (cons (cons tvar ty) subst)))
        
        )
