(module test-gen-equations (lib "eopl.ss" "eopl")
        
        (require "lang.scm")
        (require "gen-equations.scm")
        
        ; [t0=bool, t1=t0, t2=int]
        (define exp1
          (if-exp (const-exp 3) (const-exp 3) (const-exp 3)))
        
        (display (gen-equations exp1))
        
        )
