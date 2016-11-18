#lang eopl

(require "utils.rkt")

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
   (tree red-blue-tree?)))

;; (define-datatype bintree bintree? 
;;    (leaf-node 
;;     (num integer?))
;;    (interior-node
;;     (key symbol?) 
;;     (left bintree?)
;;     (right bintree?)))

;(define mark-leaves-with-red-depth
;  (lambda (tree)))

(define tree-1 (red-node (leaf 5) (leaf 10)))
(report-unit-tests-completed 'red-blue-tree)
