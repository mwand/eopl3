#lang eopl

(require "lang.rkt")                  ; for expression?

(provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  (list-val
   (li list?)))

;;; extractors:

(define expval->num
  (lambda (v)
    (cases expval v
           (num-val (num) num)
           (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
           (bool-val (bool) bool)
           (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
           (proc-val (proc) proc)
           (else (expval-extractor-error 'proc v)))))

(define expval->list
  (lambda (v)
    (cases expval v
           (list-val (li) li)
           (else (expval-extractor-error 'list)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

(define expval->denval
  (lambda (v)
    (cases expval v
           (num-val (n) n)
           (bool-val (b) b)
           (proc-val (p) p)
           (list-val (l) l))))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

;; Page: 148
(define identifier? symbol?)

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
   (saved-cont continuation?))
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (let-head-cont
   (vars (list-of identifier?))
   (exps (list-of expression?))
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (let2-exp-cont
   (var1 identifier?)
   (var2 identifier?)
   (exp2 expression?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (let3-exp-cont
   (var1 identifier?)
   (var2 identifier?)
   (exp2 expression?)
   (var3 identifier?)
   (exp3 expression?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (diff2-cont
   (val1 expval?)
   (saved-cont continuation?))
  (rator-cont
   (rands (list-of expression?))
   (saved-env environment?)
   (saved-cont continuation?))
  (rands-cont
   (val1 expval?)
   (val2 (list-of expval?))
   (rest (list-of expression?))
   (saved-env environment?)
   (saved-cont continuation?))
  (cons-cont
   (tail expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (lst-head-cont
   (tail (list-of expression?))
   (saved-env environment?)
   (saved-cont continuation?))
  (lst-tail-cont
   (val1 expval?)
   (saved-cont continuation?)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env*
   (bvars (list-of symbol?))
   (bvals (list-of expval?))
   (saved-env environment?))
  (extend-env-rec*
   (p-names (list-of symbol?))
   (b-vars (list-of (list-of symbol?)))
   (p-bodies (list-of expression?))
   (saved-env environment?)))


