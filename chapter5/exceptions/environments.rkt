#lang eopl

(require "data-structures.rkt")
(provide init-env empty-env extend-env extend-env* extend-env-rec* apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> environment

;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;;; represent environment as an alist  ((id rhs) ...)

;;; rhs is either an expval or a list (bvar body)
;;; expval is for extend-env; the list is for extend-env-rec.

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

(define empty-env
  (lambda ()
    '()))

(define empty-env?
  (lambda (x) (null? x)))

(define extend-env
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define extend-env*
  (lambda (syms vals old-env)
    (cons (list syms vals) old-env)))

(define extend-env-rec*
  (lambda (p-name b-var p-body saved-env)
    (cons
     (list p-name b-var p-body)
     saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ([bindings (car env)]
               [ids (list-ref bindings 0)]
               [expval-or-bvars (list-ref bindings 1)])
          (cond
            [(list? ids)
             (let ([n (location search-sym ids)])
               (if n
                   (let ([val (list-ref expval-or-bvars n)])
                     ;;(not (symbol? val))`
                     ;; (if (not (andmap symbol? val))
                     (if (expval? val)
                         val
                         ;; pnames, letrec, extend-env-rec*
                         (proc-val (procedure val
                                              (list-ref (list-ref bindings 2) n)
                                              env))))
                   (apply-env (cdr env) search-sym)))]
            [(not (eqv? search-sym ids))
             (apply-env (cdr env) search-sym)]
            ;; [(not (symbol? expval-or-bvars))
            [else
             ;; this was built by extend-env
             expval-or-bvars])))))
            ;; [else
            ;;  (display "there")
            ;;  ;; this was built by extend-env-rec
            ;;  (let ((bvar (cadr bindings))
            ;;        (body (caddr bindings)))
            ;;    (proc-val (procedure bvar body env)))])))))

;; location : Sym * Listof(Sym) -> Maybe(Int)
;; (location sym syms) returns the location of sym in syms or #f is
;; sym is not in syms.  We can specify this as follows:
;; if (memv sym syms)
;;   then (list-ref syms (location sym syms)) = sym
;;   else (location sym syms) = #f
(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n)
            (+ n 1)))
      (else #f))))
