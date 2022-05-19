#lang eopl

;; grammar for the LETREC language

(require "drscheme-init.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ;; ("let" identifier "=" expression "in" expression)
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)

    (expression
     ("let2" identifier "=" expression
             identifier "=" expression "in" expression)
     let2-exp)

    (expression
     ("let3" identifier "=" expression
             identifier "=" expression
             identifier "=" expression "in" expression)
     let3-exp)

    (expression
     ;; ("proc" "(" identifier ")" expression)
     ("proc" "(" (separated-list identifier "," )")" expression)
     proc-exp)

    (expression
     ;; ("(" expression expression ")")
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("letrec"
      (arbno
       identifier "(" (separated-list identifier ",") ")" "=" expression)
      "in" expression)
     letrec-exp)

    (expression
     ("emptylist")
     emptylist-exp)

    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)

    (expression
     ("car" "(" expression ")")
     car-exp)

    (expression
     ("cdr" "(" expression ")")
     cdr-exp)

    (expression
     ("null?" "(" expression ")")
     null?-exp)

    (expression
     ("list" "(" (separated-list expression ",") ")")
     list-exp)

    ;; new for implicit-refs
    (expression
     ("set" identifier "=" expression)
     assign-exp)

    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))


