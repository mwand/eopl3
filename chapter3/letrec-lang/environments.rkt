#lang eopl
;; builds environment interface, using data structures defined in
;; data-structures.scm.

(require "data-structures.rkt")

(provide init-env empty-env extend-env extend-env* extend-env-rec-vector apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; usage: (init-env) = [i=1, v=5, x=10]
;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.
;; Page: 69

;; (define init-env
;;   (lambda ()
;;     (extend-env
;;      'i (num-val 1)
;;      (extend-env
;;       'v (num-val 5)
;;       (extend-env
;;        'x (num-val 10)
;;        (empty-env))))))
(define init-env empty-env)

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;; Page: 86
(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (eopl:error 'apply-env "No binding for ~s" search-sym))
           (extend-env (var val saved-env)
                       (if (eqv? search-sym var)
                           (if (expval? val)
                               val
                               (vector-ref val 0))
                           (apply-env saved-env search-sym)))
           (extend-env* (vars vals saved-env)
                        ;; (begin (display vars)
                        ;;        (newline)
                        ;;        (display vals)
                        ;;        (newline)
                        ;;        (display env)
                        ;;        (newline)
                        ;;        (display "============")
                        ;;        )
                        (if (or (null? vars) (null? vals))
                            (apply-env saved-env search-sym)
                            (if (eqv? search-sym (car vars))
                                (if (expval? (car vals))
                                    (car vals)
                                    (vector-ref vals 0))
                                (apply-env 
                                 (extend-env* (cdr vars)
                                              (cdr vals)
                                              saved-env)
                                 search-sym))))
           (extend-env-rec (vars vec idx saved-env)
                           (if (null? vars)
                               (apply-env saved-env search-sym)
                               (if (eqv? search-sym (car vars))
                                   ;; (begin (display vec)
                                   ;;        (newline))
                                   (vector-ref vec idx)
                                   (apply-env
                                    (extend-env-rec
                                     (cdr vars)
                                     vec
                                     (+ idx 1)
                                     saved-env)
                                    search-sym))))
           )))

;; helper func for interp usage.
(define extend-env-rec-vector
  (lambda (p-names b-vars p-bodies saved-env)
    (let* ([len (length p-names)]
           [vec (make-vector len)]
           [new-env (extend-env-rec p-names vec 0 saved-env)])
      (let build-proc-vector ([b-vars b-vars]
                              [p-bodies p-bodies]
                              [n 0])
        ;; (begin (display p-names)
        ;;        (newline)
        ;;        (display b-vars)
        ;;        (newline)
        ;;        (display p-bodies)
        ;;        (newline))
        (unless (or (null? p-names) (null? b-vars) (null? p-bodies))
          (vector-set! vec n
                       (proc-val (procedure (car b-vars)
                                            (car p-bodies)
                                            new-env)))
          (build-proc-vector (cdr b-vars)
                             (cdr p-bodies)
                             (+ n 1))))
        new-env)))
