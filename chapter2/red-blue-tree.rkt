#lang racket

(require eopl)
(require rackunit)

;; ex 2.26 red-blue-tree datatype
;; Red-blue-tree ::= Red-blue-subtree
;; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;;                  ::= (blue-node {Red-blue-subtree}∗)
;;                  ::= (leaf-node Int)
;; (define-datatype red-blue-tree red-blue-tree?
;;   (empty-red-blue-tree)
;;   (not-empty-red-blue-tree
;;    (node red-blue-subtree?)))

(define-datatype red-blue-tree red-blue-tree?
  (leaf (num integer?))
  (red-node
   (left red-blue-tree?)
   (rigth red-blue-tree?))
  (blue-node
   (tree (list-of red-blue-tree?))))

(define mark-leaves-with-red-depth-rec
  (lambda (tree count)
    (cases red-blue-tree tree
           (leaf (num) (leaf (+ 0 count)))
           (red-node
            (left right)
            (red-node (mark-leaves-with-red-depth-rec left (+ count 1))
                      (mark-leaves-with-red-depth-rec right (+ count 1))))
           (blue-node
            (tree)
            (blue-node (map (lambda (x)
                              (mark-leaves-with-red-depth-rec x count))
                            tree)))
           )))

(define mark-leaves-with-red-depth
  (lambda (tree)
    (mark-leaves-with-red-depth-rec tree 0)))

(define tree-1 (red-node (leaf 5) (leaf 10)))
(define tree-2 (blue-node (list tree-1 (leaf 20))))
(define tree-3 (red-node tree-2 tree-2))

(module+ test
  (check-equal? (mark-leaves-with-red-depth tree-3)
                (red-node (blue-node (list (red-node (leaf 2) (leaf 2)) (leaf 1)))
                          (blue-node (list (red-node (leaf 2) (leaf 2)) (leaf 1)))))
  )
