(module store (lib "eopl.ss" "eopl")
        
        (require racket/base)

        (provide (all-defined-out))
        
        (define empty-store
          (lambda () '()))
        
        (define get-store
          (lambda () the-store))
        
        (define initialize-store!
          (lambda () set! the-store (empty-store)))
        
        (define reference?
          (lambda (v) (integer? v)))
        
        (define newref
          (lambda (val)
            (let ((next-ref (length the-store)))
              (set! the-store (append the-store (list val)))
              next-ref)))
        
        (define deref
          (lambda (ref)
            (list-ref the-store ref)))
        
        (define setref!
          (lambda (ref val)
            (set! the-store
                  (letrec
                   ((setref-inner
                     (lambda (store1 ref1)
                       (cond
                        ((null? store1)
                         (report-invalid-reference ref the-store))
                        ((zero? ref1)
                         (cons val (cdr store1)))
                        (else
                         (cons
                          (car store1)
                          (setref-inner
                           (cdr store1) (- ref1 1))))))))
                   (setref-inner the-store ref)))))
        
        (define report-invalid-reference
          (lambda (ref store)
            (eopl:error 'invalid-reference
                        "Invalid reference ~s in store ~%"
                        ref
                        store)))
        )
