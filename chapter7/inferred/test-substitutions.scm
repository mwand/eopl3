(module tests (lib "eopl.ss" "eopl")
        
        (require "substitutions.scm")
        (require "lang.scm")
        
        ; [t0=bool, t1=t0, t2=int]
        (define subst
          (extend-subst-ex7.18
           (extend-subst-ex7.18
            (extend-subst-ex7.18 (empty-subst) (tvar-type 0) (bool-type))
            (tvar-type 1) (tvar-type 0))
           (tvar-type 2) (int-type)))
        
        (apply-subst-to-type-ex7.18 (tvar-type 1) subst)
        
        )
