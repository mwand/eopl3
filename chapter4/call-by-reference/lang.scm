(module lang (lib "eopl.ss" "eopl")                

  ;; language for CALL-BY-REFERENCE.  Based on MUTABLE-PAIRS.
  
  (require "drscheme-init.scm")
  
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
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
        ("letrec"
          (arbno identifier "(" identifier ")" "=" expression)
           "in" expression)
        letrec-exp)
      
      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      (expression
        ("set" identifier "=" expression)
        assign-exp)

      (expression
        ("newpair" "(" expression "," expression ")")
        newpair-exp)

      (expression
        ("left" "(" expression ")")
        left-exp)

      (expression
        ("setleft" expression "=" expression)
        setleft-exp)

      (expression
        ("right" "(" expression ")")
        right-exp)

      (expression
        ("setright" expression "=" expression)
        setright-exp)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
