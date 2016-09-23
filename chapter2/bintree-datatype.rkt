#lang eopl

(require "utils.rkt")

;; ex 2.24, 2.25
(define-datatype bintree bintree? 
  (leaf-node 
   (num integer?))
  (interior-node
   (key symbol?) 
   (left bintree?)
   (right bintree?)))

(define bintree-to-list
  (lambda (t)
    (cases bintree t
           (leaf-node (num) (cons 'leaf-node
                                  (cons num '())))
           (interior-node
            (key left right)
            (cons 'interior-node
                  (cons key
                        (cons (bintree-to-list left)
                              (cons (bintree-to-list right) '()))))))))

(define sum-of-interior
  (lambda (t)
    (cases bintree t
           (leaf-node (num) num)
           (interior-node
            (key left right)
            (+ (sum-of-interior left)
               (sum-of-interior right))))))

(define max-interior
  (lambda (t)
    (cases bintree t
           (leaf-node (num) (eopl:error "a leaf node"))
           (interior-node
            (key left right)
            (cases bintree left
                  (leaf-node
                   (num)
                   (cases bintree right
                          (leaf-node (num) key)
                          (interior-node
                           (rkey rleft rright)
                           (cond
                             [(> num 0) (max-interior
                                         (interior-node key rleft rright))]
                             [else
                              (max-interior right)]))))
                  (interior-node
                   (lkey lleft rright)
                   (cases bintree right
                          (leaf-node
                           (num)
                           (cond
                             [(> num 0) (max-interior
                                         (interior-node key lleft rright))]
                             [else
                              (max-interior left)]))
                          (interior-node
                           (rkey rleft rright)
                           (max-interior
                            (interior-node key
                                           (max-interior left)
                                           (max-interior right)))))))
              ))))

(equal?? (bintree-to-list
          (interior-node
           'a
           (leaf-node 3)
           (leaf-node 4)))
         '(interior-node a (leaf-node 3) (leaf-node 4)))

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -2) tree-1))
(define tree-3
  (interior-node 'baz (leaf-node 3) tree-2))
(define tree-4
  (interior-node 'bax tree-3 (leaf-node 4)))

(max-interior tree-3)

(report-unit-tests-completed 'bintree-datatype)
