#lang eopl

;; input language for the CPS converter.

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
     ("+" "(" (separated-list expression ",") ")")
     sum-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     ("number?" "(" expression ")")
     number?-exp)

    (expression
     ("equal?" "(" expression "," expression ")")
     equal?-exp)

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

    (expression
     ("letrec"
      (arbno identifier "(" (arbno identifier) ")"
             "=" expression)
      "in"
      expression)
     letrec-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" (arbno identifier) ")" expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
