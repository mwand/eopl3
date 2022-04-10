#lang eopl

(require "data-structures.rkt")
(require "store.rkt")
(provide init-env empty-env extend-env apply-env has-binding?)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; (init-env) builds an environment in which:
;; i is bound to a location containing the expressed value 1,
;; v is bound to a location containing the expressed value 5, and
;; x is bound to a location containing the expressed value 10.
(define init-env
  (lambda ()
    (extend-env
     'i (newref (num-val 1))
     (extend-env
      'v (newref (num-val 5))
      (extend-env
       'x (newref (num-val 10))
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define apply-env
  (lambda (env search-var)
    (cases environment env
           (empty-env ()
                      (eopl:error 'apply-env "No binding for ~s" search-var))
           (extend-env (bvar bval saved-env)
                       (if (eqv? search-var bvar)
                           bval
                           (apply-env saved-env search-var)))
           (extend-env* (bvars bvals saved-env)
                        (cond
                          [(null? bvars)
                           (apply-env saved-env search-var)]
                          [(eqv? search-var (car bvars))
                           (car bvals)]
                          [else
                           (apply-env (extend-env*
                                       (cdr bvars)
                                       (cdr bvals)
                                       saved-env)
                                      search-var)]))
           (extend-env-rec* (p-names b-vars p-bodies saved-env)
                            (let* ([n (location search-var p-names)]
                                   [proc-vec (make-vector (length p-names))]
                                   [proc-ref (map newref p-names)]
                                   ;; [proc-ref (map newref (vector->list proc-vec))]
                                   [new-env
                                    (extend-env* p-names
                                                 proc-ref
                                                 saved-env)])
                              (begin (map setref! proc-ref
                                          (map (lambda (v b)
                                                 (proc-val
                                                  (procedure v b
                                                             (extend-env* p-names proc-ref new-env))))
                                               b-vars
                                               p-bodies)))
                              ;; n : (maybe int)
                              (if n
                                  (list-ref proc-ref n)
                                  (apply-env
                                   (extend-env* p-names proc-ref saved-env)
                                   search-var)))))))

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



;; had-binding? : Env * Sym -> Maybe(Sym)
(define empty-env? null?)

(define has-binding?
  (lambda (env search-sym)
    (cases environment env
           (empty-env () #f)
           (extend-env (bvar bval saved-env)
                       (if (eqv? search-sym bvar)
                           search-sym
                           (has-binding? saved-env search-sym)))
           (extend-env* (bvars bvals saved-env)
                        (if (member search-sym bvars)
                            search-sym
                            (has-binding? saved-env search-sym)))
           (extend-env-rec* (p-names b-vars p-bodies saved-env)
                            (if (or (member search-sym p-names)
                                    (member search-sym b-vars))
                                search-sym
                                (has-binding? saved-env search-sym))))))
    ;; (cond
    ;;   [(equal? (empty-env) env) #f]
    ;;   [(eqv? search-sym (car env)) search-sym]
    ;;   [else
    ;;    (has-binding? (cdr env) search-sym)])))
