#lang eopl

;; grammar for the LET language


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
     ("+" "(" expression "," expression ")")
     addition-exp)

    (expression
     ("minus" "(" expression ")")
     minus-exp)

    (expression
     ("*" "(" expression "," expression ")")
     multiplication-exp)

    (expression
     ("/" "(" expression "," expression ")")
     quotient-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("equal?" "(" expression "," expression ")")
     equal?-exp)

    (expression
     ("greater?" "(" expression "," expression ")")
     greater?-exp)

    (expression
     ("less?" "(" expression "," expression ")")
     less?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

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
     ("cond" "{"
             ;; (separated-list expression "==>" expression) "}" )
             (arbno expression "==>" expression) "}" )
     cond-exp)
    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

