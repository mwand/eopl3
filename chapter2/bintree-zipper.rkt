#lang racket
(require rackunit)

;; acording to http://blog.ezyang.com/2010/04/you-could-have-invented-zippers/
;; http://www.cs.umd.edu/class/spring2019/cmsc388F/lectures/zippers.html
;; https://wiki.haskell.org/Zipper This is real understandable

(define bintree
  (lambda (n left right)
    (list n left right)))

(define bintree->val car)

(define bintree->lson cadr)

(define bintree->rson caddr)

;; in link, that is loc
(define zipperTree
  (lambda (bt context)
    (list bt context)))

(define zipperTree->bintree car)

(define zipperTree->ctx cadr)
;; context which is a list
(define context
  (lambda (type val tree ctx)
    (list type val tree ctx)))

(define context->type car)
(define context->val cadr)
(define context->tree caddr)
(define context->ctx cadddr)

;; current-element, move-to-left-son, move-to-right-son, at-leaf?, insert-to-left, and insert-to-right.
;; move-up, at-root?, and at-leaf?.
(define number->zippertree
  (lambda (n)
    (zipperTree
     (bintree n '() '())
     (context 'top ;; top node
              n
              '() ;; bintree is empty
              '() ;; context also empty
              ))))

(define current-element
  (lambda (zt)
    (bintree->val (zipperTree->bintree zt))))

(define move-to-left-son
  (lambda (zt)
    (let* ([bt (zipperTree->bintree zt)]
           [lson (bintree->lson bt)]
           [rson (bintree->rson bt)]
           [val (bintree->val bt)]
           [ctx (zipperTree->ctx zt)])
          (zipperTree lson (context 'left val rson ctx)))
    ))

(define move-to-right-son
  (lambda (zt)
    (let* ([bt (zipperTree->bintree zt)]
           [lson (bintree->lson bt)]
           [rson (bintree->rson bt)]
           [val (bintree->val bt)]
           [ctx (zipperTree->ctx zt)])
      (zipperTree rson (context 'right val lson ctx)))
    ))

(define (move-up zt)
  (let* ([ctx (zipperTree->ctx zt)]
         [type (context->type ctx)]
         [tree (context->tree ctx)]
         [new-ctx (context->ctx ctx)]
         [val (context->val ctx)]
         [tree-current (zipperTree->bintree zt)])
    (cond
      [(eqv? type 'right)
       (zipperTree (bintree val tree tree-current) new-ctx)]
      [(eqv? type 'left)
       (zipperTree (bintree val tree-current tree) new-ctx)]
      [(eqv? type 'top) zt]
      [else ;; top
       (error "not valid type: ~s~%" type)])))

(define at-leaf?
  (lambda (zt)
    (let* ([bt (zipperTree->bintree zt)]
           [lson (bintree->lson bt)]
           [rson (bintree->rson bt)])
      (and (null? lson) (null? rson)))))

(define (at-root? zt)
  (let* ([ctx (zipperTree->ctx zt)]
         [type (context->type ctx)])
    (eqv? type 'top)))

(define (insert-to-left n zt)
  (let* ([bt (zipperTree->bintree zt)]
         [ctx (zipperTree->ctx zt)]
         [lson (bintree->lson bt)]
         [rson (bintree->rson bt)]
         [val (bintree->val bt)])
    (cond
      [(null? lson)
       (zipperTree (bintree val
                            (bintree n '() '())
                            rson)
                   ctx)]
      [else
       (zipperTree (bintree val
                            (bintree n lson '())
                            rson)
                   ctx)]
      )))

(define (insert-to-right n zt)
  (let* ([bt (zipperTree->bintree zt)]
         [ctx (zipperTree->ctx zt)]
         [lson (bintree->lson bt)]
         [rson (bintree->rson bt)]
         [val (bintree->val bt)])
    (cond
      [(null? rson)
       (zipperTree (bintree val
                            lson
                            (bintree n '() '()))
                   ctx)]
      [else
       (zipperTree (bintree val
                            lson
                            (bintree n rson '()))
                   ctx)]
      )))

(define t1 (insert-to-right 3 (insert-to-left 2 (number->zippertree 1))))

(module+ test
  (check-equal? (move-to-left-son t1) '((2 () ()) (left 1 (3 () ()) (top 1 () ()))))
  (check-equal? (move-to-right-son t1) '((3 () ()) (right 1 (2 () ()) (top 1 () ()))))
  (check-equal? (move-up (move-to-left-son t1)) t1)
  (check-equal? (move-up (move-to-right-son t1)) t1)
  (check-true (at-root? t1))
  (check-false (at-leaf? t1))
  (check-true (at-leaf? (move-to-left-son t1)))
  (check-true (at-leaf? (move-to-right-son t1)))
  (check-false (at-root? (move-to-right-son t1)))
  )
