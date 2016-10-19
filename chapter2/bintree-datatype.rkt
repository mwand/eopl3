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

(define rec-max-interior
  (lambda (t)
    (cases bintree t
           (leaf-node (num) (list '() num))
           (interior-node
            (key left right)
            (let ([l-rec (rec-max-interior left)]
                  [r-rec (rec-max-interior right)])
              (list 
               (cond
                 [(null? (car l-rec))
                  (cond
                    [(null? (car r-rec))
                     (list key (+ (cadr l-rec) (cadr r-rec)))]
                    [else
                     (list key (+ (cadr l-rec) (cadar r-rec)))])]
                 [else
                  (list key (+ (cadar l-rec) (cadar r-rec)))])
               l-rec
               r-rec))))))

;; (define max-interior
;;   (lambda (t)
;;     (let ([tree (rec-max-interior t)]
;;           (cond
;;             [(null? (car tree)) (eopl:error "This is a leaf node")]
;;             (cond
;;               [(null? (caadr tree))
;;                (cond
;;                  [(null? (caaddr tree)) (caar tree)]
;;                  [else
;;                   (if )])]))))))

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
