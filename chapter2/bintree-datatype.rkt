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

;; return a list (key-max, sum-max, current-sum)
(define rec-max-interior
  (lambda (t)
    (cases bintree t
           (leaf-node (num) (list '() num num))
           (interior-node
            (key left right)
            (let* ([l-rec (rec-max-interior left)]
                   [r-rec (rec-max-interior right)]
                   [l-max-sum-key (car l-rec)]
                   [l-max-sum (cadr l-rec)]
                   [l-current-sum (caddr l-rec)]
                   [r-max-sum-key (car r-rec)]
                   [r-max-sum (cadr r-rec)]
                   [r-current-sum (caddr r-rec)]
                   [sum (+ l-current-sum r-current-sum)])
              (cond
                [(and (null? (car l-rec)) (> sum r-max-sum)) (list key sum sum)]
                [(and (null? (car l-rec)) (< sum r-max-sum)) (list r-max-sum-key r-max-sum sum)]
                [(and (null? (car r-rec)) (> sum l-max-sum)) (list key sum sum)]
                [(and (null? (car r-rec)) (> sum r-max-sum)) (list l-max-sum-key l-max-sum sum)]
                [(and (> sum l-max-sum) (> sum r-max-sum)) (list key sum sum)]
                [(and (> sum l-max-sum) (< sum r-max-sum)) (list r-max-sum-key r-max-sum sum)]
                [(and (< sum l-max-sum) (> sum r-max-sum)) (list l-max-sum-key l-max-sum sum)]
                [else
                 (cond
                   [(> l-max-sum r-max-sum) (list l-max-sum-key l-max-sum sum)]
                   [else
                    (list r-max-sum-key r-max-sum sum)])]))))))

(define (max-interior node)
  (car (rec-max-interior node)))

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
(define tree-6
  (interior-node 'bar (leaf-node -10) tree-1))
(define tree-5 (interior-node 'zz tree-6 tree-3))

(max-interior tree-3)

(equal?? (max-interior tree-3) 'baz)
(equal?? (max-interior tree-5) 'baz)

(report-unit-tests-completed 'bintree-datatype)

;; (define (max-interior-rec node)
;;   (cases bintree node
;;     (leaf-node (num) (list '() num num))
;;     (interior-node (key left right)
;;                    (let ((left-ret (max-interior-rec left))
;;                          (right-ret (max-interior-rec right)))
;;                      (let ((sum (+ (caddr left-ret)
;;                                    (caddr right-ret))))
;;                        (if (and (or (null? (car left-ret))
;;                                     (>= sum (cadr left-ret)))
;;                                 (or (null? (car right-ret))
;;                                     (>= sum (cadr right-ret))))
;;                            (list key sum sum)
;;                            (if (and (not (null? (car left-ret)))
;;                                     (or (null? (car right-ret))
;;                                         (>= (cadr left-ret) (cadr right-ret))))
;;                                (list (car left-ret) (cadr left-ret) sum)
;;                                (list (car right-ret) (cadr right-ret) sum))))))))

