(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for proc-lang/ds-rep

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (var symbol?)
      (body expression?)
      (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

;;;;;;;;;;;;;;;;;;;;;;list of free variables;;;;;;;;;;;;;;;;;;
(define (free-vars-of exp)

  (define (remove item seq)
    (if (null? seq)
        '()
        (let ((rests (remove item (cdr seq)))
              (first (car seq)))
          (if (eq? item first)
              rests
              (cons first rests)))))

  
  (lambda (var)

    (define (strip-shadow-from body)
      (lambda (var1)
        (let ((free-vars ((free-vars-of body) var1)))
            (if (eq? var1 var)
                free-vars
                (remove var free-vars)))))

    (cases expression exp
        (const-exp (num) '())

        (var-exp (var1) (if (eq? var1 var) '() (list var1)))

        (diff-exp (exp1 exp2)
          (append ((free-vars-of exp1) var)
                  ((free-vars-of exp2) var)))

        (zero?-exp (exp1)
          ((free-vars-of exp1) var))
          
        (if-exp (exp1 exp2 exp3)
          (append ((free-vars-of exp1) var)
                  (append ((free-vars-of exp2) var)
                          ((free-vars-of exp3) var))))

        (let-exp (var1 exp1 body)
          ((strip-shadow-from body) var1))

        
        (proc-exp (var1 body)
          ((strip-shadow-from body) var1))

        (call-exp (rator rand)
          (append ((free-vars-of rator) var)
                  ((free-vars-of rand) var)))

        )))
)
