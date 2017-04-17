(module data-structures (lib "eopl.ss" "eopl")
        
        (provide (all-defined-out))               ; too many things to list
        
        (define (reduce fn list init)
          (if (null? list) init
            (fn (car list)
                (reduce fn (cdr list) init))))
        
        )
