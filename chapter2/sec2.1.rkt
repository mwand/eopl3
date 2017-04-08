(module sec2.1 (lib "eopl.ss" "eopl")

  (require "utils.rkt")

  (let ()
    ;; Unary Representation
    ;; page 33
    (define zero (lambda () '()))
    (define is-zero? (lambda (n) (null? n)))
    (define successor (lambda (n) (cons #t n)))
    (define predecessor (lambda (n) (cdr n)))

    ;; Need this style of definition to define a recursive function
    ;; inside a let, sorry.
    (define (plus x y)
      (if (is-zero? x)
        y
        (successor (plus (predecessor x) y))))

    (define (scheme-int->my-int n)
      (if (zero? n) (zero)
        (successor (scheme-int->my-int (- n 1)))))

    (define (my-int->scheme-int x)
      (if (is-zero? x) 0
        (+ 1 (my-int->scheme-int (predecessor x)))))

    (equal?? 
      (my-int->scheme-int
        (plus 
          (scheme-int->my-int 3)
          (scheme-int->my-int 7)))
      10)

    (report-unit-tests-completed 'unary-representation)
    )

  (let ()
    ;; Scheme number representation
    ;; page 33
    (define zero (lambda () 0))
    (define is-zero? (lambda (n) (zero? n)))
    (define successor (lambda (n) (+ n 1)))
    (define predecessor (lambda (n) (- n 1)))

    (define (plus x y)
      (if (is-zero? x)
        y
        (successor (plus (predecessor x) y))))

    (equal?? (plus 3 7) 10)

    (report-unit-tests-completed 'scheme-number-representation)

    )

  (let ()
    ;; Reverse-number representation
    ;; Represent n by the Scheme number 5-n
    (define zero (lambda () 5))
    (define is-zero? (lambda (n) (= n 5)))
    (define successor (lambda (n) (- n 5)))
    (define predecessor (lambda (n) (+ n 5)))

    ;; unchanged below here!

    (define plus
      (lambda (x y)
        (if (is-zero? x)
          y
          (successor (plus (predecessor x) y)))))

    (define (scheme-int->my-int n)
        (if (zero? n) (zero)
          (successor (scheme-int->my-int (- n 1)))))

    (define (my-int->scheme-int x)
        (if (is-zero? x) 0
          (+ 1 (my-int->scheme-int (predecessor x)))))

    (equal?? 
      (my-int->scheme-int
        (plus 
          (scheme-int->my-int 3)
          (scheme-int->my-int 7)))
      10)

    (report-unit-tests-completed 'reverse-number-representation)
    )

  )

