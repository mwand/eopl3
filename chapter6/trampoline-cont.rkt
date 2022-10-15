#lang racket

(require rackunit)
(require eopl)
(require racket/trace)

(define sym 'uninitialized)
(define lst 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define pc 'uninitialized)


(define remove-fst
  (lambda (arg-sym arg-lst)
    (set! cont (end-cont))
    (set! sym arg-sym)
    (set! lst arg-lst)
    (set! pc remove-fst/k)
    (trampoline!)
    val))
    ;; (remove-fst/k)))

(define trampoline!
  (lambda ()
    (when pc
      (begin (pc)
             (trampoline!)))))

(define remove-fst/k
  (lambda ()
    (if (null? lst)
        (begin (set! val '())
               (set! pc apply-cont))
        (if (eqv? (car lst) sym)
            (begin (set! val (cdr lst))
                   (set! pc apply-cont))
            ;; (apply-cont))
            (begin (set! cont (remove-fst1-cont (car lst) cont))
                   (set! lst (cdr lst))
                   (set! pc remove-fst/k))))))
                   ;; (remove-fst/k))))))

(define-datatype continuation continuation?
  (end-cont)
  (remove-fst1-cont
   (head symbol?)
   (saved-cont continuation?)))

(define apply-cont
  (lambda ()
    (cases continuation cont
           (end-cont ()
                     (begin
                       (eopl:printf "End of computation.~%")
                       (eopl:printf "This sentence should appear only once.~%")
                       (set! pc #f)))
                       ;; val))
           (remove-fst1-cont (head saved-cont)
                             (set! cont saved-cont)
                             (set! val (cons head val))
                             (set! pc apply-cont)))))
                             ;; (apply-cont)))))

(module+ test
  (check-equal? (remove-fst 'a '(a b c)) '(b c))
  (check-equal? (remove-fst 'b '(e f g)) '(e f g))
  (check-equal? (remove-fst 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
  (check-equal? (remove-fst 'x '()) '())
  )
;; (trace remove-fst remove-fst/k apply-cont)
