#lang eopl

(require "utils.rkt")

(define one (lambda () '(one)))
(define diff
  (lambda (lson rson) (list lson rson)))

;; (define diff-tree
;;   (lambda (dt)
;;     (cond
;;       [(null? dt) '()]
;;       [(equal? dt (one)) (one)]
;;       [else
;;        (cons (diff-tree (car dt))
;;              (diff-tree (cdr dt)))])))

(define zero (lambda () (diff (one) (one))))
(define is-zero? (lambda (n) (equal? 0
                                     (diff-tree n))))
(define successor (lambda (n)
                    (diff n
                          (diff (diff (one) (one))
                                (one)))))
(define predecessor (lambda (n)
                      (diff n (one))))

(define diff-tree
  (lambda (dt)
    (cond
      [(null? dt) 0]
      [(equal? dt (one)) 1]
      [else
       (- (diff-tree (car dt))
          (diff-tree (cdr dt)))])))

(define diff-plus-plus
  (lambda (a b)
    (diff a (diff (zero) b))))

(equal?? (diff-tree (diff (one)
                          (diff (one) (one)))) 1)

(equal?? (diff-tree (successor (diff (one)
                                      (diff (one) (one))))) 2)

(equal?? (diff-tree (predecessor 
                     (successor (diff (one)
                                      (diff (one) (one)))))) 1)
