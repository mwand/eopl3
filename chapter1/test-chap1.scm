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
         (if (not (equal? observed-ans correct-ans))
           (printf "~s returned ~s, should have returned ~s~%"
             'test-exp
             observed-ans
             correct-ans))))))


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
  ;; (equal?? (nth-element '(a b c d) 2) 'foo)
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

  ;; leaf : Int -> Bintree
  ;; Page: 29
  (define leaf
    (lambda (n)
      (tag 'leaf (list n))))

  ;; interior-node : Symbol * Bintree * Bintree -> Bintree
  ;; Page : 29
  (define interior-node
    (lambda (sym left right)
      (tag 'interior (list sym left right))))

  ;; leaf? : Bintree -> Bool
  ;; Page : 29
  (define leaf?
    (lambda (tree)
      (eq? 'leaf (car tree))))

  (define tag
    (lambda (t contents)
      (list t contents)))
  (interior-node 's (leaf 5) (leaf 6))
  ;; lson : Bintree -> Bintree
  ;; Page : 29
  (define lson
    (lambda (node)
      (cadr (cadr node))))

  ;; rson : Bintree -> Bintree
  ;; Page : 29
  (define rson
    (lambda (node)
      (caddr (cadr node))))

  ;; contents-of : Bintree -> List
  ;; Page : 29
  (define contents-of
    (lambda (tree)
      (cadr tree)))

  ;; symbol : Bintree -> List
  (define symbol-of
    (lambda (node)
      (car (cadr node))))
  
  ;; double-tree : Bintree -> Bintree
  ;; Page : 29
  (define double-tree
    (lambda (tree)
      (if (leaf? tree)
          (leaf (map (lambda (x) (* x 2))
                     (contents-of tree)))
          (interior-node (symbol-of tree)
                         (double-tree (lson tree))
                         (double-tree (rson tree))))))
  
  ;; mark-leaves-with-red-depth : Bintree -> Bintree
  ;; Page : 29
  (define mark-leaves-with-red-depth
    (lambda (tree)
      (do-mark tree 0)))
  (define do-mark
    (lambda (tree depth-of-parent)
      (let* ((sym (symbol-of tree))
             (depth (+ depth-of-parent
                       (if (eq? 'red sym) 1 0))))
        (if (leaf? tree)
            (leaf depth)
            (interior-node sym
                           (do-mark (lson tree) depth)
                           (do-mark (rson tree) depth))))))
  ;; path : Integer * BinarySearchTree -> Listof(Symbol)
  ;; Page : 30
  (define path
    (lambda (n bst)
      (reverse (find bst n '()))))
  (define find
    (lambda (bst n known)
      (cond ((null? bst)
             (error "No path found"))
            ((< n (car bst))
             (find (cadr bst) n (cons 'left known)))
            ((> n (car bst))
             (find (caddr bst) n (cons 'right known)))
            (else known))))
  
  ;; number-leaves : Bintree -> Bintree
  ;; Page : 30
  (define number-leaves
    (lambda (tree)
      (car (do-number tree 0))))
  (define do-number
    (lambda (tree start)
      (if (leaf? tree)
          (cons (leaf start) 1)
          (let* ((left-result (do-number (lson tree) start))
                 (left-count (cdr left-result))
                 (left-tree (car left-result))
                 (right-result (do-number (rson tree) (+ start left-count)))
                 (right-count (cdr right-result))
                 (right-tree (car right-result)))
            (cons (interior-node (symbol-of tree)
                                 left-tree
                                 right-tree)
                  (+ left-count right-count))))))
  
  (define number-elements
    (lambda (lst)
      (if (null? lst) '()
          (g (list 0 (car lst)) (number-elements (cdr lst))))))
  (define g
    (lambda (first rests)
      (if (null? rests)
          (list first)
          (cons first
                (g ((lambda (x y)
                      (list (+ x 1) y))
                    (caar rests)
                    (cadar rests))
                   (cdr rests))))))
  )    