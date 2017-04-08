#lang eopl

(require "utils.rkt")

(define leaf
  (lambda (x)
    (cond
      [(number? x) x]
      [else
       (eopl:error 'leaf "Not a number.")])))

(define interior-node
  (lambda (content lson rson)
    (cond
      [(number? content) (list content lson rson)]
      [else
       (eopl:error 'interior-node "not a symbol")])))

(define leaf?
  (lambda (x)
    (number? x)))

(define lson
  (lambda (node)
    (cadr node)))

(define rson
  (lambda (node)
    (caddr node)))

(define contents-of
  (lambda (node)
    (cond
      [(leaf? node) (leaf node)]
      [else
       (car node)])))

(define number->bintree
  (lambda (n)
    (interior-node n '() '())))

(define current-element
  (lambda (node)
    (car node)))

(define move-to-left
  (lambda (node)
    (cond
      [(leaf? node) (eopl:error "not a interior-node, it is a leaf")]
      [else
       (cadr node)])))

(define move-to-right
   (lambda (node)
    (caddr node)))

(define at-leaf?
  (lambda (node)
    (cond
      [(null? node) #t]
      [(leaf? node) #t]
      [else #f])))

(define insert-to-left
  (lambda (n node)
    (cond
      [(null? (lson node))
       (list (contents-of node) (number->bintree n) (rson node))]
      [else
       (list (contents-of node)
             (list n (lson node) '())
             (rson node))])))

(define insert-to-right
  (lambda (n node)
    (cond
      [(null? (rson node))
       (list (contents-of node) (lson node) (number->bintree n))]
      [else
       (list (contents-of node)
             (lson node)
             (list n (rson node) '()))])))

(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))

(equal?? (move-to-left t1) '(12 () ()))
(equal?? (current-element (move-to-left t1)) 12)
(equal?? (at-leaf? (move-to-right (move-to-left t1))) #t)
(equal?? (insert-to-left 15 t1)
         '(13
           (15
            (12 () ())
            ())
           (14 () ())))

(report-unit-tests-completed "bintree-rep.rkt")
