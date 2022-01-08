#lang racket
(require rackunit)


(define sequence
  (lambda (n left right)
    (list n left right)))

(define number->sequence
  (lambda (n)
    (sequence n '() '())))

(define current-element
  (lambda (seq)
    (car seq)))

(define sequence->left
  (lambda (seq)
    (cadr seq)))

(define sequence->right
  (lambda (seq)
    (caddr seq)))

(define at-left-end?
  (lambda (seq)
    (null? (cadr seq))))

(define at-right-end?
  (lambda (seq)
    (null? (caddr seq))))

(define move-to-left
  (lambda (seq)
    (if (at-left-end? seq)
        (error "at the left end, could'n move left")
        (sequence (car (sequence->left seq))
                  (cdr (sequence->left seq))
                  (cons (current-element seq) (sequence->right seq)))
        )))

(define move-to-right
  (lambda (seq)
    (if (at-right-end? seq)
        (error "at the right end, couldn't move right")
        (sequence (car (sequence->right seq))
                  (cons (current-element seq) (sequence->left seq))
                  (cdr (sequence->right seq))))))

(define insert-to-left
  (lambda (n seq)
    (sequence (current-element seq)
              (cons n (sequence->left seq))
              (sequence->right seq))))

(define insert-to-right
  (lambda (n seq)
    (sequence (current-element seq)
              (sequence->left seq)
              (cons n (sequence->right seq))
              )))


(module+ test
  (define seq1
    (insert-to-left 5
                    (insert-to-left 4
                                    (insert-to-left 3
                                                    (insert-to-left 2
                                                                    (insert-to-left 1
                                                                                    (insert-to-right 7
                                                                                                     (insert-to-right 8
                                                                                                                      (insert-to-right 9
                                                                                                                                       (number->sequence 6))))))))))

  (check-eqv? (current-element seq1) 6)
  (check-equal? (move-to-left seq1) '(5 (4 3 2 1) (6 7 8 9)))
  (check-equal? (move-to-right seq1) '(7 (6 5 4 3 2 1) (8 9)))
  (check-exn exn:fail:contract (move-to-right (move-to-right (move-to-right (move-to-right seq1)))))
  ;; (check-exn exn:fail (move-to-right (move-to-right (move-to-right seq1))))
  (move-to-right (move-to-right (move-to-right seq1)))
  )

