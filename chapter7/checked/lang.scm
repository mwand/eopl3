(module lang (lib "eopl.ss" "eopl")
        
        ;; grammar for the CHECKED language
        
        (require "drscheme-init.scm")
        (require "../../utils.scm")
        
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
            
            (expression
             ("if2" expression "then" expression "else" expression)
             if2-exp)
            
            (expression (identifier) var-exp)
            
            (expression
             ("let" (arbno identifier "=" expression) "in" expression)
             let-exp)
            
            (expression
             ("proc" "(" (arbno identifier ":" type) ")" expression)
             proc-exp)
            
            (expression
             ("(" expression (arbno expression) ")")
             call-exp)
            
            (expression
             ("letrec"
              (arbno type identifier "(" identifier ":" type ")" "=" expression)
              "in" expression)
             letrec-exp)
            
            ;; Page: 245
            ;; ex7.8
            (expression
             ("newpair" "(" expression "," expression ")")
             pair-exp)
            
            (expression
             ("unpair" identifier identifier "=" expression "in" expression)
             unpair-exp)
            
            (type
             ("int")
             int-type)
            
            (type
             ("bool")
             bool-type)
            
            (type
             ("(" (separated-list type "*") "->" type ")")
             proc-type)
            
            ;; ex7.8
            (type
             ("pairof" type "*" type)
             pair-type)
            ))
        
        ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
        
        (sllgen:make-define-datatypes the-lexical-spec the-grammar)
        
        (define show-the-datatypes
          (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
        
        (define scan&parse
          (sllgen:make-string-parser the-lexical-spec the-grammar))
        
        (define just-scan
          (sllgen:make-string-scanner the-lexical-spec the-grammar))
        
        ;;;;;;;;;;;;;;;; type-to-external-form ;;;;;;;;;;;;;;;;
        
        ;; type-to-external-form : Type -> List
        ;; Page: 243
        (define type-to-external-form
          (lambda (ty)
            (cases type ty
                   (int-type () 'int)
                   (bool-type () 'bool)
                   ;; Page: 243
                   ;; ex7.5
                   ;; (type * type * ... * type -> type)
                   (proc-type (arg-type-list result-type)
                              (define format-type-list
                                (lambda (type-list)
                                  (cdr (reduce (lambda (element lst) (cons '* (cons element lst)))
                                               type-list
                                               '()))))
                              (append
                               (format-type-list (map type-to-external-form arg-type-list))
                               (list
                                '->
                                (type-to-external-form result-type))))
                   ;; Page: 245
                   ;; ex7.8
                   ;; pairof type * type
                   (pair-type (ty1 ty2)
                              (list
                               'pairof
                               (type-to-external-form ty1)
                               (type-to-external-form ty2))))))
        
        (define pair-type->fst
          (lambda (ty)
            (cases type ty
                   (pair-type (ty1 ty2) ty1)
                   (else (eopl:error "Looking for a pair-type, found ~s" ty)))))
        
        (define pair-type->snd
          (lambda (ty)
            (cases type ty
                   (pair-type (ty1 ty2) ty2)
                   (else (eopl:error "Looking for a pair-type, found ~s" ty)))))
        
        )
