(module test-chap1 mzscheme

  ;; This collects the code in chapter 1.  It uses a very primitive
  ;; testing macro, equal??.  This needs to be a macro because we print
  ;; the test as well as evaluate it.

  ;; We use a more sophisticated testing setup for the interpreters
  ;; later on.

  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (when (not (equal? observed-ans correct-ans))
           (printf "~s returned ~s, should have returned ~s~%"
             'test-exp
             observed-ans
             correct-ans)
           )))))


  ;; in-S? : N -> Bool
  ;; usage: (in-S? n) = #t if n is in S, #f otherwise
  ;; The set S is defined in Definition 1.1.1 on page 2.
  (define in-S?
    (lambda (n)
      (if (zero? n) #t
        (if (>= (- n 3) 0) (in-S? (- n 3))
          #f))))

  (equal?? (in-S? 4) #f)
  (equal?? (in-S? 9) #t)


  ;; list-length : List -> Int
  ;; usage: (list-length l) = the length of l
  ;; Page: 14
  (define list-length
    (lambda (lst)
      (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

  (equal?? (list-length '(a (b c) d)) 3)


  ;; nth-element : List * Int -> SchemeVal
  ;; usage: (nth-element lst n) = the nth element of lst
  ;; Page: 15
  (define nth-element
    (lambda (lst n)
      (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
          (car lst)
          (nth-element (cdr lst) (- n 1))))))

  (define report-list-too-short
    (lambda (n)
      (error 'nth-element 
        "List too short by ~s elements.~%" (+ n 1))))

  ;; uncomment these to test equal??
  (equal?? (nth-element '(a b c d) 2) 'c)
  ;; (equal?? (nth-element '(a b c d) 3) 'bar)

  (equal?? (nth-element '(a b c d) 2) 'c)

  ;; remove-first : Sym * Listof(Sym) -> Listof(Sym)
  ;; Page: 18
  (define remove-first
    (lambda (s los)
      (if (null? los)
        '()
        (if (eqv? (car los) s)
          (cdr los)
          (cons (car los) (remove-first s (cdr los)))))))

  (equal?? (remove-first 'a '(a b c)) '(b c))

  (equal?? (remove-first 'b '(e f g)) '(e f g))

  (equal?? (remove-first 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))

  (equal?? (remove-first 'x '()) '())

  ;; occurs-free? : Sym * Lcexp -> Bool
  ;; usage:
  ;;   returns #t if the symbol var occurs free in exp,
  ;;   otherwise returns #f.
  ;; Page: 19
  (define occurs-free?
    (lambda (var exp)
      (cond
        ((symbol? exp) (eqv? var exp))
        ((eqv? (car exp) 'lambda)
         (and
           (not (eqv? var (car (cadr exp))))
           (occurs-free? var (caddr exp))))
        (else
          (or
            (occurs-free? var (car exp))
            (occurs-free? var (cadr exp)))))))


  (equal?? (occurs-free? 'x 'x) #t)

  (equal?? (occurs-free? 'x 'y) #f)

  (equal?? (occurs-free? 'x '(lambda (x) (x y))) #f)

  (equal?? (occurs-free? 'x '(lambda (y) (x y))) #t)

  (equal?? (occurs-free? 'x '((lambda (x) x) (x y))) #t)

  (equal?? (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))) #t)

  ;; subst : Sym * Sym * S-list -> S-list
  ;; Page: 21
  (define subst
    (lambda (new old slist)
      (if (null? slist)
        '()
        (cons
          (subst-in-s-exp new old (car slist)) 
          (subst new old (cdr slist))))))

  ;; subst-in-s-exp : Sym * Sym * S-exp -> S-exp
  ;; Page: 21
  (define subst-in-s-exp
    (lambda (new old sexp)
      (if (symbol? sexp) 
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))

  (equal?? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))

  ;; number-elements-from : Listof(SchemeVal) * Int ->
  ;;                           Listof(List(Int,SchemeVal))  
  ;; usage: (number-elements-from '(v0 v1 v2  ...) n) 
  ;;         = ((n v0 ) (n+1 v1) (n+2 v2) ...)
  ;; Page: 23
  (define number-elements-from
    (lambda (lst n)
      (if (null? lst) '()
        (cons
          (list n (car lst))
          (number-elements-from (cdr lst) (+ n 1))))))

  ;; number-elements : List -> Listof(List(Int,SchemeVal))
  ;; Page: 23.
  (define number-elements
    (lambda (lst)
      (number-elements-from lst 0)))

  (equal?? (number-elements '(a b c d e)) '((0 a) (1 b) (2 c) (3 d) (4 e)))

  ;; list-sum : Listof(Int) -> Int
  ;; Page: 24
  (define list-sum
    (lambda (loi)
      (if (null? loi)
        0
        (+ (car loi) 
          (list-sum (cdr loi))))))

  (equal?? (list-sum (list 1 2 3 4 5)) 15)

  ;; partial-vector-sum : Vectorof(Int) * Int -> Int
  ;; usage if 0 <= n < length(v), then
  ;;            (partial-vector-sum v n) = SUM(v_i from 0 <= i <= n)
  ;; Page: 25
  (define partial-vector-sum
    (lambda (v n)
      (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
          (partial-vector-sum v (- n 1))))))

  ;; vector-sum : Vectorof(Int) -> Int
  ;; usage (vector-sum v) = SUM(v_i from 0 <= i <= length(v)-1)
  ;; Page: 25
  (define vector-sum
    (lambda (v)
      (let ((n (vector-length v)))
        (if (zero? n)
          0
          (partial-vector-sum v (- n 1))))))

  (equal?? (vector-sum (vector 1 2 3 4 5)) 15)

  ;; ex 1.26
  (define up
    (lambda (lst)
      (if
        (null? lst) '()
        (cond
          [(list? (car lst))
           (cons (caar lst) (up (merge (cdr (car lst)) (cdr lst))))]
          [else (cons (car lst) (up (cdr lst)))])
        )
        ))

  (define append-my
    (lambda (lst elem)
      (cond
        [(null? elem) lst]
        [(null? lst) (list elem)]
        [else
         (cond
           [(null? (cdr lst)) (cons (car lst) (list elem))]
           [else
            (cons (car lst) (append-my (cdr lst) elem))])])))

  (equal?? (append-my '(1 2)  3) '(1 2 3))

  (define merge
    (lambda (lst1 lst2)
      (cond
        [(null? lst2) lst1]
        [(null? lst1) lst2]
        [else (merge (append-my lst1 (car lst2)) (cdr lst2))]
        )
      ))

  (equal?? (merge '(1 2 (34)) '(3 4)) '(1 2 (34) 3 4))


  (equal?? (up '((1 2) (3 4))) '(1 2 3 4))

  ;; ex 1.22
  (define filter-in
    (lambda (pred lst)
      (cond
        [(null? lst) '()]
        [(pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)))]
        [else (filter-in pred (cdr lst))])))

  (equal?? (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
  (equal?? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))

  ;; ex 1.27
  (define flatten
    (lambda (lst)
      (cond
        [(null? lst) '()]
        [else
         (cond
           [(null? (car lst)) (flatten (cdr lst))]
           [(list? (car lst))
            (merge (flatten (car lst))
                      (flatten (cdr lst)))]
            ;;(merge (flatten (car (car lst)))
            ;;          (flatten (merge (cdr (car lst)) (cdr lst))))]
           [else (cons (car lst) (flatten (cdr lst)))]
           )])
      ))

  (equal?? (flatten '(() a b ((((c)))) (d () ((e)) x (y z)))) '(a b c d e x y z))
  (equal?? (flatten '((a) () (b ()) () (c))) '(a b c))

  ;; ex 1.29
  (define last
    (lambda (lst)
      (cond
        [(null? lst) '()]
        [(null? (cdr lst)) (car lst)]
        [else
         (last (cdr lst))])))

  (define sort
    (lambda (loi)
      (cond
        [(null? loi) '()]
        [(null? (cdr loi)) loi]
        [else
         (merge (sort (filter-in (lambda (x) (> (car loi) x)) (cdr loi)))
                (cons (car loi)
                      (sort (filter-in
                             (lambda (x) (<= (car loi) x)) (cdr loi)))))])))

  ;; Bintree ::= Int | (Symbol Bintree Bintree)
  ;; ex1.31
  (define leaf
    (lambda (x)
      (cond
        [(number? x) x]
        [else
         (error 'leaf "Not a number.")])))

  (define interior-node
    (lambda (content lson rson)
      (cond
        [(symbol? content) (list content lson rson)]
        [else
         (error 'interior-node "not a symbol")])))

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

  ;; ex 1.33
  (define leaf-plus
    (lambda (n tree)
      (cond
        [(leaf? tree) (+ (leaf tree) n)]
        [(leaf? (lson tree))
         (interior-node (contents-of tree)
                        (+ (lson tree) n)
                        (leaf-plus n (rson tree)))]
        [(leaf? (rson tree))
         (interior-node (contents-of tree)
                        (leaf-plus n (lson tree))
                        (+ n (rson tree)))]
        [else
         (interior-node (contents-of tree)
                        (leaf-plus n (lson tree))
                        (leaf-plus n (rson tree)))])))

  (define leaf-plus-one
    (lambda (tree)
      (leaf-plus 1 tree)))

  (define mark-leaves-with-red-depth
    (lambda (tree)
      (cond
        [(leaf? tree) (leaf 0)]
        [(equal? (contents-of tree) 'red)
         (interior-node 'red
                        (leaf-plus-one (mark-leaves-with-red-depth (lson tree)))
                        (leaf-plus-one (mark-leaves-with-red-depth (rson tree))))]
        [else
         (interior-node (contents-of tree)
                        (mark-leaves-with-red-depth (lson tree))
                        (mark-leaves-with-red-depth (rson tree)))])))

  (equal?? 
   (mark-leaves-with-red-depth
    (interior-node 'red
                   (interior-node 'bar
                                  (leaf 26)
                                  (leaf 12))
                   (interior-node 'red
                                  (leaf 11)
                                  (interior-node 'quux
                                                 (leaf 117)
                                                 (leaf 14)))))
   '(red (bar 1 1) (red 2 (quux 2 2))))


  ;; ex 1.34
  ;; Binary-search-tree ::= () | (Int Binary-search-tree Binary-search-tree)
  (define path
    (lambda (n bst)
      (cond
        [(null? bst) '()]
        [(equal? (car bst) n) '()]
        [(< n (car bst)) (cons 'left (path n (cadr bst)))]
        [else
         (cons 'right (path n (caddr bst)))])))

  (equal?? (path 17 '(14 (7 () (12 () ()))
                         (26 (20 (17 () ())
                                 ())
                             (31 () ()))))
           '(right left left))

  ;; ex 1.35
  (define number-leaves
    (lambda (tree)
      (cond
        [(leaf? tree) (leaf 0)]
        [(leaf? (lson tree))
         (interior-node (car tree)
                        (number-leaves (cadr tree))
                        (leaf-plus-one (number-leaves (caddr tree))))]
        [else
         (interior-node (car tree)
                        (number-leaves (cadr tree))
                        (leaf-plus 2 (number-leaves (caddr tree))))]
        )))

  (equal?? (number-leaves
            (interior-node 'red
                           (interior-node 'bar
                                          (leaf 26)
                                          (leaf 12))
                           (interior-node 'red
                                          (leaf 11)
                                          (interior-node 'quux
                                                         (leaf 117)
                                                         (leaf 14)))))
           '(red (bar 0 1) (red 2 (quux 3 4))))

  ;; ex 1.36
  ;; number-elements-from : Listof(SchemeVal) × Int → Listof(List(Int, SchemeVal))
  ;; usage: (number-elements-from ’(v0 v1 v2 ...) n)
  ;; = ((n v0) (n+1 v1) (n+2 v2) ...)
  ;; (define number-elements-from
  ;;   (lambda (lst n)
  ;;     (if (null? lst) ’()
  ;;         (cons
  ;;          (list n (car lst))
  ;;          (number-elements-from (cdr lst) (+ n 1))))))

  ;; number-elements : List → Listof(List(Int, SchemeVal))
  ;; (define number-elements
  ;;   (lambda (lst)
  ;;     (number-elements-from lst 0)))

  (define number-elements-1
    (lambda (lst)
      (cond
        [(null? lst) '()]
        [(g (list 0 (car lst)) (number-elements-1 (cdr lst)))])))

  (define g
    (lambda (elem lst)
      (cond
        [(null? lst) (list elem)]
        ;; [(null? (cdr lst)) (cons elem '())]
        [else
         (cons elem
               (first-plus-one lst))])))

  (define first-plus-one
    (lambda (lst)
      (cond
        [(null? lst) '()]
        [else
         (cons (cons (+ 1 (caar lst)) (cdar lst))
               (first-plus-one (cdr lst)))])))

  (equal?? (number-elements-1 '(a b c d))
           '((0 a) (1 b) (2 c) (3 d)))
 )
