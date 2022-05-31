;; full trace of example on pp 173-177.

  (text-example-1.2
      "let index 
            = proc (n)
               letrec inner2 (lst)
                 % find position of n in lst else raise error
                 % exception 
                  = if null?(lst) then raise 99       
                    else if zero?(-(car(lst),n)) then 0
                    else let v = (inner2 cdr(lst))
                         in -(v,-1)
               in proc (lst)
                   try (inner2 lst)
                   catch (x) -1
       in ((index 5) list(2, 3))"
      -1)


Welcome to DrScheme, version 299.400p1.
Language: (module ...).
drscheme-init.scm plt209.1.5 10feb2005
lecture09/exceptions/interp.scm 15-Mar-06
|(value-of/k
   #(struct:let-exp
     index
     #(struct:proc-exp
       n
       #(struct:letrec-exp
         inner2
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         #(struct:proc-exp
           lst
           #(struct:try-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:var-exp lst))
             x
             #(struct:const-exp -1)))))
     #(struct:call-exp
       #(struct:call-exp
         #(struct:var-exp index)
         #(struct:const-exp 5))
       #(struct:const-list-exp (2 3))))
   ((i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:end-cont))
|(value-of/k
   #(struct:call-exp
     #(struct:proc-exp
       index
       #(struct:call-exp
         #(struct:call-exp
           #(struct:var-exp index)
           #(struct:const-exp 5))
         #(struct:const-list-exp (2 3))))
     #(struct:proc-exp
       n
       #(struct:letrec-exp
         inner2
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         #(struct:proc-exp
           lst
           #(struct:try-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:var-exp lst))
             x
             #(struct:const-exp -1))))))
   ((i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:end-cont))
|(value-of/k
   #(struct:proc-exp
     index
     #(struct:call-exp
       #(struct:call-exp
         #(struct:var-exp index)
         #(struct:const-exp 5))
       #(struct:const-list-exp (2 3))))
   ((i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rator-cont
     #(struct:proc-exp
       n
       #(struct:letrec-exp
         inner2
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         #(struct:proc-exp
           lst
           #(struct:try-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:var-exp lst))
             x
             #(struct:const-exp -1)))))
     ((i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont)))
|(apply-cont
   #(struct:rator-cont
     #(struct:proc-exp
       n
       #(struct:letrec-exp
         inner2
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         #(struct:proc-exp
           lst
           #(struct:try-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:var-exp lst))
             x
             #(struct:const-exp -1)))))
     ((i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont))
   #(struct:proc-val
     #(struct:procedure
       index
       #(struct:call-exp
         #(struct:call-exp
           #(struct:var-exp index)
           #(struct:const-exp 5))
         #(struct:const-list-exp (2 3)))
       ((i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10))))))
|(value-of/k
   #(struct:proc-exp
     n
     #(struct:letrec-exp
       inner2
       lst
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:null?-unop)
           #(struct:var-exp lst))
         #(struct:raise-exp #(struct:const-exp 99))
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:zero?-unop)
             #(struct:diff-exp
               #(struct:unop-exp
                 #(struct:car-unop)
                 #(struct:var-exp lst))
               #(struct:var-exp n)))
           #(struct:const-exp 0)
           #(struct:diff-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:unop-exp
                 #(struct:cdr-unop)
                 #(struct:var-exp lst)))
             #(struct:const-exp -1))))
       #(struct:proc-exp
         lst
         #(struct:try-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:var-exp lst))
           x
           #(struct:const-exp -1)))))
   ((i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         index
         #(struct:call-exp
           #(struct:call-exp
             #(struct:var-exp index)
             #(struct:const-exp 5))
           #(struct:const-list-exp (2 3)))
         ((i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:end-cont)))
|(apply-cont
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         index
         #(struct:call-exp
           #(struct:call-exp
             #(struct:var-exp index)
             #(struct:const-exp 5))
           #(struct:const-list-exp (2 3)))
         ((i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:end-cont))
   #(struct:proc-val
     #(struct:procedure
       n
       #(struct:letrec-exp
         inner2
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         #(struct:proc-exp
           lst
           #(struct:try-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:var-exp lst))
             x
             #(struct:const-exp -1))))
       ((i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10))))))
|(value-of/k
   #(struct:call-exp
     #(struct:call-exp
       #(struct:var-exp index)
       #(struct:const-exp 5))
     #(struct:const-list-exp (2 3)))
   ((index
     #(struct:proc-val
       #(struct:procedure
         n
         #(struct:letrec-exp
           inner2
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           #(struct:proc-exp
             lst
             #(struct:try-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:var-exp lst))
               x
               #(struct:const-exp -1))))
         ((i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10))))))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:end-cont))
|(value-of/k
   #(struct:call-exp #(struct:var-exp index) #(struct:const-exp 5))
   ((index
     #(struct:proc-val
       #(struct:procedure
         n
         #(struct:letrec-exp
           inner2
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           #(struct:proc-exp
             lst
             #(struct:try-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:var-exp lst))
               x
               #(struct:const-exp -1))))
         ((i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10))))))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rator-cont
     #(struct:const-list-exp (2 3))
     ((index
       #(struct:proc-val
         #(struct:procedure
           n
           #(struct:letrec-exp
             inner2
             lst
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:null?-unop)
                 #(struct:var-exp lst))
               #(struct:raise-exp #(struct:const-exp 99))
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:zero?-unop)
                   #(struct:diff-exp
                     #(struct:unop-exp
                       #(struct:car-unop)
                       #(struct:var-exp lst))
                     #(struct:var-exp n)))
                 #(struct:const-exp 0)
                 #(struct:diff-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:unop-exp
                       #(struct:cdr-unop)
                       #(struct:var-exp lst)))
                   #(struct:const-exp -1))))
             #(struct:proc-exp
               lst
               #(struct:try-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:var-exp lst))
                 x
                 #(struct:const-exp -1))))
           ((i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10))))))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont)))
|(value-of/k
   #(struct:var-exp index)
   ((index
     #(struct:proc-val
       #(struct:procedure
         n
         #(struct:letrec-exp
           inner2
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           #(struct:proc-exp
             lst
             #(struct:try-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:var-exp lst))
               x
               #(struct:const-exp -1))))
         ((i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10))))))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rator-cont
     #(struct:const-exp 5)
     ((index
       #(struct:proc-val
         #(struct:procedure
           n
           #(struct:letrec-exp
             inner2
             lst
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:null?-unop)
                 #(struct:var-exp lst))
               #(struct:raise-exp #(struct:const-exp 99))
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:zero?-unop)
                   #(struct:diff-exp
                     #(struct:unop-exp
                       #(struct:car-unop)
                       #(struct:var-exp lst))
                     #(struct:var-exp n)))
                 #(struct:const-exp 0)
                 #(struct:diff-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:unop-exp
                       #(struct:cdr-unop)
                       #(struct:var-exp lst)))
                   #(struct:const-exp -1))))
             #(struct:proc-exp
               lst
               #(struct:try-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:var-exp lst))
                 x
                 #(struct:const-exp -1))))
           ((i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10))))))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:rator-cont
       #(struct:const-list-exp (2 3))
       ((index
         #(struct:proc-val
           #(struct:procedure
             n
             #(struct:letrec-exp
               inner2
               lst
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:null?-unop)
                   #(struct:var-exp lst))
                 #(struct:raise-exp #(struct:const-exp 99))
                 #(struct:if-exp
                   #(struct:unop-exp
                     #(struct:zero?-unop)
                     #(struct:diff-exp
                       #(struct:unop-exp
                         #(struct:car-unop)
                         #(struct:var-exp lst))
                       #(struct:var-exp n)))
                   #(struct:const-exp 0)
                   #(struct:diff-exp
                     #(struct:call-exp
                       #(struct:var-exp inner2)
                       #(struct:unop-exp
                         #(struct:cdr-unop)
                         #(struct:var-exp lst)))
                     #(struct:const-exp -1))))
               #(struct:proc-exp
                 lst
                 #(struct:try-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:var-exp lst))
                   x
                   #(struct:const-exp -1))))
             ((i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10))))))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(apply-cont
   #(struct:rator-cont
     #(struct:const-exp 5)
     ((index
       #(struct:proc-val
         #(struct:procedure
           n
           #(struct:letrec-exp
             inner2
             lst
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:null?-unop)
                 #(struct:var-exp lst))
               #(struct:raise-exp #(struct:const-exp 99))
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:zero?-unop)
                   #(struct:diff-exp
                     #(struct:unop-exp
                       #(struct:car-unop)
                       #(struct:var-exp lst))
                     #(struct:var-exp n)))
                 #(struct:const-exp 0)
                 #(struct:diff-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:unop-exp
                       #(struct:cdr-unop)
                       #(struct:var-exp lst)))
                   #(struct:const-exp -1))))
             #(struct:proc-exp
               lst
               #(struct:try-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:var-exp lst))
                 x
                 #(struct:const-exp -1))))
           ((i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10))))))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:rator-cont
       #(struct:const-list-exp (2 3))
       ((index
         #(struct:proc-val
           #(struct:procedure
             n
             #(struct:letrec-exp
               inner2
               lst
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:null?-unop)
                   #(struct:var-exp lst))
                 #(struct:raise-exp #(struct:const-exp 99))
                 #(struct:if-exp
                   #(struct:unop-exp
                     #(struct:zero?-unop)
                     #(struct:diff-exp
                       #(struct:unop-exp
                         #(struct:car-unop)
                         #(struct:var-exp lst))
                       #(struct:var-exp n)))
                   #(struct:const-exp 0)
                   #(struct:diff-exp
                     #(struct:call-exp
                       #(struct:var-exp inner2)
                       #(struct:unop-exp
                         #(struct:cdr-unop)
                         #(struct:var-exp lst)))
                     #(struct:const-exp -1))))
               #(struct:proc-exp
                 lst
                 #(struct:try-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:var-exp lst))
                   x
                   #(struct:const-exp -1))))
             ((i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10))))))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont)))
   #(struct:proc-val
     #(struct:procedure
       n
       #(struct:letrec-exp
         inner2
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         #(struct:proc-exp
           lst
           #(struct:try-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:var-exp lst))
             x
             #(struct:const-exp -1))))
       ((i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10))))))
|(value-of/k
   #(struct:const-exp 5)
   ((index
     #(struct:proc-val
       #(struct:procedure
         n
         #(struct:letrec-exp
           inner2
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           #(struct:proc-exp
             lst
             #(struct:try-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:var-exp lst))
               x
               #(struct:const-exp -1))))
         ((i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10))))))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         n
         #(struct:letrec-exp
           inner2
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           #(struct:proc-exp
             lst
             #(struct:try-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:var-exp lst))
               x
               #(struct:const-exp -1))))
         ((i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:rator-cont
       #(struct:const-list-exp (2 3))
       ((index
         #(struct:proc-val
           #(struct:procedure
             n
             #(struct:letrec-exp
               inner2
               lst
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:null?-unop)
                   #(struct:var-exp lst))
                 #(struct:raise-exp #(struct:const-exp 99))
                 #(struct:if-exp
                   #(struct:unop-exp
                     #(struct:zero?-unop)
                     #(struct:diff-exp
                       #(struct:unop-exp
                         #(struct:car-unop)
                         #(struct:var-exp lst))
                       #(struct:var-exp n)))
                   #(struct:const-exp 0)
                   #(struct:diff-exp
                     #(struct:call-exp
                       #(struct:var-exp inner2)
                       #(struct:unop-exp
                         #(struct:cdr-unop)
                         #(struct:var-exp lst)))
                     #(struct:const-exp -1))))
               #(struct:proc-exp
                 lst
                 #(struct:try-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:var-exp lst))
                   x
                   #(struct:const-exp -1))))
             ((i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10))))))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(apply-cont
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         n
         #(struct:letrec-exp
           inner2
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           #(struct:proc-exp
             lst
             #(struct:try-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:var-exp lst))
               x
               #(struct:const-exp -1))))
         ((i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:rator-cont
       #(struct:const-list-exp (2 3))
       ((index
         #(struct:proc-val
           #(struct:procedure
             n
             #(struct:letrec-exp
               inner2
               lst
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:null?-unop)
                   #(struct:var-exp lst))
                 #(struct:raise-exp #(struct:const-exp 99))
                 #(struct:if-exp
                   #(struct:unop-exp
                     #(struct:zero?-unop)
                     #(struct:diff-exp
                       #(struct:unop-exp
                         #(struct:car-unop)
                         #(struct:var-exp lst))
                       #(struct:var-exp n)))
                   #(struct:const-exp 0)
                   #(struct:diff-exp
                     #(struct:call-exp
                       #(struct:var-exp inner2)
                       #(struct:unop-exp
                         #(struct:cdr-unop)
                         #(struct:var-exp lst)))
                     #(struct:const-exp -1))))
               #(struct:proc-exp
                 lst
                 #(struct:try-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:var-exp lst))
                   x
                   #(struct:const-exp -1))))
             ((i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10))))))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont)))
   #(struct:num-val 5))
|(value-of/k
   #(struct:letrec-exp
     inner2
     lst
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:null?-unop)
         #(struct:var-exp lst))
       #(struct:raise-exp #(struct:const-exp 99))
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:zero?-unop)
           #(struct:diff-exp
             #(struct:unop-exp
               #(struct:car-unop)
               #(struct:var-exp lst))
             #(struct:var-exp n)))
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1))))
     #(struct:proc-exp
       lst
       #(struct:try-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:var-exp lst))
         x
         #(struct:const-exp -1))))
   ((n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rator-cont
     #(struct:const-list-exp (2 3))
     ((index
       #(struct:proc-val
         #(struct:procedure
           n
           #(struct:letrec-exp
             inner2
             lst
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:null?-unop)
                 #(struct:var-exp lst))
               #(struct:raise-exp #(struct:const-exp 99))
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:zero?-unop)
                   #(struct:diff-exp
                     #(struct:unop-exp
                       #(struct:car-unop)
                       #(struct:var-exp lst))
                     #(struct:var-exp n)))
                 #(struct:const-exp 0)
                 #(struct:diff-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:unop-exp
                       #(struct:cdr-unop)
                       #(struct:var-exp lst)))
                   #(struct:const-exp -1))))
             #(struct:proc-exp
               lst
               #(struct:try-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:var-exp lst))
                 x
                 #(struct:const-exp -1))))
           ((i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10))))))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont)))
|(value-of/k
   #(struct:proc-exp
     lst
     #(struct:try-exp
       #(struct:call-exp
         #(struct:var-exp inner2)
         #(struct:var-exp lst))
       x
       #(struct:const-exp -1)))
   ((inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rator-cont
     #(struct:const-list-exp (2 3))
     ((index
       #(struct:proc-val
         #(struct:procedure
           n
           #(struct:letrec-exp
             inner2
             lst
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:null?-unop)
                 #(struct:var-exp lst))
               #(struct:raise-exp #(struct:const-exp 99))
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:zero?-unop)
                   #(struct:diff-exp
                     #(struct:unop-exp
                       #(struct:car-unop)
                       #(struct:var-exp lst))
                     #(struct:var-exp n)))
                 #(struct:const-exp 0)
                 #(struct:diff-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:unop-exp
                       #(struct:cdr-unop)
                       #(struct:var-exp lst)))
                   #(struct:const-exp -1))))
             #(struct:proc-exp
               lst
               #(struct:try-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:var-exp lst))
                 x
                 #(struct:const-exp -1))))
           ((i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10))))))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont)))
|(apply-cont
   #(struct:rator-cont
     #(struct:const-list-exp (2 3))
     ((index
       #(struct:proc-val
         #(struct:procedure
           n
           #(struct:letrec-exp
             inner2
             lst
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:null?-unop)
                 #(struct:var-exp lst))
               #(struct:raise-exp #(struct:const-exp 99))
               #(struct:if-exp
                 #(struct:unop-exp
                   #(struct:zero?-unop)
                   #(struct:diff-exp
                     #(struct:unop-exp
                       #(struct:car-unop)
                       #(struct:var-exp lst))
                     #(struct:var-exp n)))
                 #(struct:const-exp 0)
                 #(struct:diff-exp
                   #(struct:call-exp
                     #(struct:var-exp inner2)
                     #(struct:unop-exp
                       #(struct:cdr-unop)
                       #(struct:var-exp lst)))
                   #(struct:const-exp -1))))
             #(struct:proc-exp
               lst
               #(struct:try-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:var-exp lst))
                 x
                 #(struct:const-exp -1))))
           ((i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10))))))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont))
   #(struct:proc-val
     #(struct:procedure
       lst
       #(struct:try-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:var-exp lst))
         x
         #(struct:const-exp -1))
       ((inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10))))))
|(value-of/k
   #(struct:const-list-exp (2 3))
   ((index
     #(struct:proc-val
       #(struct:procedure
         n
         #(struct:letrec-exp
           inner2
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           #(struct:proc-exp
             lst
             #(struct:try-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:var-exp lst))
               x
               #(struct:const-exp -1))))
         ((i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10))))))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         lst
         #(struct:try-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:var-exp lst))
           x
           #(struct:const-exp -1))
         ((inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:end-cont)))
|(apply-cont
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         lst
         #(struct:try-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:var-exp lst))
           x
           #(struct:const-exp -1))
         ((inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:end-cont))
   #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
|(value-of/k
   #(struct:try-exp
     #(struct:call-exp
       #(struct:var-exp inner2)
       #(struct:var-exp lst))
     x
     #(struct:const-exp -1))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:end-cont))
|(value-of/k
   #(struct:call-exp
     #(struct:var-exp inner2)
     #(struct:var-exp lst))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:try-cont
     x
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont)))
|(value-of/k
   #(struct:var-exp inner2)
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rator-cont
     #(struct:var-exp lst)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(apply-cont
   #(struct:rator-cont
     #(struct:var-exp lst)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont)))
   #(struct:proc-val
     #(struct:procedure
       lst
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:null?-unop)
           #(struct:var-exp lst))
         #(struct:raise-exp #(struct:const-exp 99))
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:zero?-unop)
             #(struct:diff-exp
               #(struct:unop-exp
                 #(struct:car-unop)
                 #(struct:var-exp lst))
               #(struct:var-exp n)))
           #(struct:const-exp 0)
           #(struct:diff-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:unop-exp
                 #(struct:cdr-unop)
                 #(struct:var-exp lst)))
             #(struct:const-exp -1))))
       ((inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10))))))
|(value-of/k
   #(struct:var-exp lst)
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         ((inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(apply-cont
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         ((inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont)))
   #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
|(value-of/k
   #(struct:if-exp
     #(struct:unop-exp #(struct:null?-unop) #(struct:var-exp lst))
     #(struct:raise-exp #(struct:const-exp 99))
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:zero?-unop)
         #(struct:diff-exp
           #(struct:unop-exp
             #(struct:car-unop)
             #(struct:var-exp lst))
           #(struct:var-exp n)))
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1))))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:try-cont
     x
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont)))
|(value-of/k
   #(struct:unop-exp #(struct:null?-unop) #(struct:var-exp lst))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:if-test-cont
     #(struct:raise-exp #(struct:const-exp 99))
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:zero?-unop)
         #(struct:diff-exp
           #(struct:unop-exp
             #(struct:car-unop)
             #(struct:var-exp lst))
           #(struct:var-exp n)))
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1)))
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(value-of/k
   #(struct:var-exp lst)
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:unop-arg-cont
     #(struct:null?-unop)
     #(struct:if-test-cont
       #(struct:raise-exp #(struct:const-exp 99))
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:zero?-unop)
           #(struct:diff-exp
             #(struct:unop-exp
               #(struct:car-unop)
               #(struct:var-exp lst))
             #(struct:var-exp n)))
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1)))
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(apply-cont
   #(struct:unop-arg-cont
     #(struct:null?-unop)
     #(struct:if-test-cont
       #(struct:raise-exp #(struct:const-exp 99))
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:zero?-unop)
           #(struct:diff-exp
             #(struct:unop-exp
               #(struct:car-unop)
               #(struct:var-exp lst))
             #(struct:var-exp n)))
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1)))
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont))))
   #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
|(apply-cont
   #(struct:if-test-cont
     #(struct:raise-exp #(struct:const-exp 99))
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:zero?-unop)
         #(struct:diff-exp
           #(struct:unop-exp
             #(struct:car-unop)
             #(struct:var-exp lst))
           #(struct:var-exp n)))
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1)))
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont)))
   #(struct:bool-val #f))
|(value-of/k
   #(struct:if-exp
     #(struct:unop-exp
       #(struct:zero?-unop)
       #(struct:diff-exp
         #(struct:unop-exp
           #(struct:car-unop)
           #(struct:var-exp lst))
         #(struct:var-exp n)))
     #(struct:const-exp 0)
     #(struct:diff-exp
       #(struct:call-exp
         #(struct:var-exp inner2)
         #(struct:unop-exp
           #(struct:cdr-unop)
           #(struct:var-exp lst)))
       #(struct:const-exp -1)))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:try-cont
     x
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont)))
|(value-of/k
   #(struct:unop-exp
     #(struct:zero?-unop)
     #(struct:diff-exp
       #(struct:unop-exp #(struct:car-unop) #(struct:var-exp lst))
       #(struct:var-exp n)))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:if-test-cont
     #(struct:const-exp 0)
     #(struct:diff-exp
       #(struct:call-exp
         #(struct:var-exp inner2)
         #(struct:unop-exp
           #(struct:cdr-unop)
           #(struct:var-exp lst)))
       #(struct:const-exp -1))
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(value-of/k
   #(struct:diff-exp
     #(struct:unop-exp #(struct:car-unop) #(struct:var-exp lst))
     #(struct:var-exp n))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:unop-arg-cont
     #(struct:zero?-unop)
     #(struct:if-test-cont
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1))
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(value-of/k
   #(struct:unop-exp #(struct:car-unop) #(struct:var-exp lst))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff1-cont
     #(struct:var-exp n)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:unop-arg-cont
       #(struct:zero?-unop)
       #(struct:if-test-cont
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1))
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont))))))
|(value-of/k
   #(struct:var-exp lst)
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:unop-arg-cont
     #(struct:car-unop)
     #(struct:diff1-cont
       #(struct:var-exp n)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:unop-arg-cont
         #(struct:zero?-unop)
         #(struct:if-test-cont
           #(struct:const-exp 0)
           #(struct:diff-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:unop-exp
                 #(struct:cdr-unop)
                 #(struct:var-exp lst)))
             #(struct:const-exp -1))
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont)))))))
|(apply-cont
   #(struct:unop-arg-cont
     #(struct:car-unop)
     #(struct:diff1-cont
       #(struct:var-exp n)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:unop-arg-cont
         #(struct:zero?-unop)
         #(struct:if-test-cont
           #(struct:const-exp 0)
           #(struct:diff-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:unop-exp
                 #(struct:cdr-unop)
                 #(struct:var-exp lst)))
             #(struct:const-exp -1))
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont))))))
   #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
|(apply-cont
   #(struct:diff1-cont
     #(struct:var-exp n)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:unop-arg-cont
       #(struct:zero?-unop)
       #(struct:if-test-cont
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1))
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont)))))
   #(struct:num-val 2))
|(value-of/k
   #(struct:var-exp n)
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff2-cont
     #(struct:num-val 2)
     #(struct:unop-arg-cont
       #(struct:zero?-unop)
       #(struct:if-test-cont
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1))
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont))))))
|(apply-cont
   #(struct:diff2-cont
     #(struct:num-val 2)
     #(struct:unop-arg-cont
       #(struct:zero?-unop)
       #(struct:if-test-cont
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1))
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont)))))
   #(struct:num-val 5))
|(apply-cont
   #(struct:unop-arg-cont
     #(struct:zero?-unop)
     #(struct:if-test-cont
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1))
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont))))
   #(struct:num-val -3))
|(apply-cont
   #(struct:if-test-cont
     #(struct:const-exp 0)
     #(struct:diff-exp
       #(struct:call-exp
         #(struct:var-exp inner2)
         #(struct:unop-exp
           #(struct:cdr-unop)
           #(struct:var-exp lst)))
       #(struct:const-exp -1))
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont)))
   #(struct:bool-val #f))
|(value-of/k
   #(struct:diff-exp
     #(struct:call-exp
       #(struct:var-exp inner2)
       #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst)))
     #(struct:const-exp -1))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:try-cont
     x
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont)))
|(value-of/k
   #(struct:call-exp
     #(struct:var-exp inner2)
     #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst)))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff1-cont
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(value-of/k
   #(struct:var-exp inner2)
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rator-cont
     #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst))
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(apply-cont
   #(struct:rator-cont
     #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst))
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont))))
   #(struct:proc-val
     #(struct:procedure
       lst
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:null?-unop)
           #(struct:var-exp lst))
         #(struct:raise-exp #(struct:const-exp 99))
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:zero?-unop)
             #(struct:diff-exp
               #(struct:unop-exp
                 #(struct:car-unop)
                 #(struct:var-exp lst))
               #(struct:var-exp n)))
           #(struct:const-exp 0)
           #(struct:diff-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:unop-exp
                 #(struct:cdr-unop)
                 #(struct:var-exp lst)))
             #(struct:const-exp -1))))
       ((inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10))))))
|(value-of/k
   #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst))
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         ((inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(value-of/k
   #(struct:var-exp lst)
   ((lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:unop-arg-cont
     #(struct:cdr-unop)
     #(struct:rand-cont
       #(struct:proc-val
         #(struct:procedure
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           ((inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont))))))
|(apply-cont
   #(struct:unop-arg-cont
     #(struct:cdr-unop)
     #(struct:rand-cont
       #(struct:proc-val
         #(struct:procedure
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           ((inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont)))))
   #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
|(apply-cont
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         ((inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont))))
   #(struct:list-val (#(struct:num-val 3))))
|(value-of/k
   #(struct:if-exp
     #(struct:unop-exp #(struct:null?-unop) #(struct:var-exp lst))
     #(struct:raise-exp #(struct:const-exp 99))
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:zero?-unop)
         #(struct:diff-exp
           #(struct:unop-exp
             #(struct:car-unop)
             #(struct:var-exp lst))
           #(struct:var-exp n)))
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1))))
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff1-cont
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(value-of/k
   #(struct:unop-exp #(struct:null?-unop) #(struct:var-exp lst))
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:if-test-cont
     #(struct:raise-exp #(struct:const-exp 99))
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:zero?-unop)
         #(struct:diff-exp
           #(struct:unop-exp
             #(struct:car-unop)
             #(struct:var-exp lst))
           #(struct:var-exp n)))
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1)))
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(value-of/k
   #(struct:var-exp lst)
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:unop-arg-cont
     #(struct:null?-unop)
     #(struct:if-test-cont
       #(struct:raise-exp #(struct:const-exp 99))
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:zero?-unop)
           #(struct:diff-exp
             #(struct:unop-exp
               #(struct:car-unop)
               #(struct:var-exp lst))
             #(struct:var-exp n)))
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1)))
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont))))))
|(apply-cont
   #(struct:unop-arg-cont
     #(struct:null?-unop)
     #(struct:if-test-cont
       #(struct:raise-exp #(struct:const-exp 99))
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:zero?-unop)
           #(struct:diff-exp
             #(struct:unop-exp
               #(struct:car-unop)
               #(struct:var-exp lst))
             #(struct:var-exp n)))
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1)))
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont)))))
   #(struct:list-val (#(struct:num-val 3))))
|(apply-cont
   #(struct:if-test-cont
     #(struct:raise-exp #(struct:const-exp 99))
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:zero?-unop)
         #(struct:diff-exp
           #(struct:unop-exp
             #(struct:car-unop)
             #(struct:var-exp lst))
           #(struct:var-exp n)))
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1)))
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont))))
   #(struct:bool-val #f))
|(value-of/k
   #(struct:if-exp
     #(struct:unop-exp
       #(struct:zero?-unop)
       #(struct:diff-exp
         #(struct:unop-exp
           #(struct:car-unop)
           #(struct:var-exp lst))
         #(struct:var-exp n)))
     #(struct:const-exp 0)
     #(struct:diff-exp
       #(struct:call-exp
         #(struct:var-exp inner2)
         #(struct:unop-exp
           #(struct:cdr-unop)
           #(struct:var-exp lst)))
       #(struct:const-exp -1)))
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff1-cont
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(value-of/k
   #(struct:unop-exp
     #(struct:zero?-unop)
     #(struct:diff-exp
       #(struct:unop-exp #(struct:car-unop) #(struct:var-exp lst))
       #(struct:var-exp n)))
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:if-test-cont
     #(struct:const-exp 0)
     #(struct:diff-exp
       #(struct:call-exp
         #(struct:var-exp inner2)
         #(struct:unop-exp
           #(struct:cdr-unop)
           #(struct:var-exp lst)))
       #(struct:const-exp -1))
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(value-of/k
   #(struct:diff-exp
     #(struct:unop-exp #(struct:car-unop) #(struct:var-exp lst))
     #(struct:var-exp n))
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:unop-arg-cont
     #(struct:zero?-unop)
     #(struct:if-test-cont
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1))
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont))))))
|(value-of/k
   #(struct:unop-exp #(struct:car-unop) #(struct:var-exp lst))
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff1-cont
     #(struct:var-exp n)
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:unop-arg-cont
       #(struct:zero?-unop)
       #(struct:if-test-cont
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1))
         ((lst #(struct:list-val (#(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:diff1-cont
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont)))))))
|(value-of/k
   #(struct:var-exp lst)
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:unop-arg-cont
     #(struct:car-unop)
     #(struct:diff1-cont
       #(struct:var-exp n)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:unop-arg-cont
         #(struct:zero?-unop)
         #(struct:if-test-cont
           #(struct:const-exp 0)
           #(struct:diff-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:unop-exp
                 #(struct:cdr-unop)
                 #(struct:var-exp lst)))
             #(struct:const-exp -1))
           ((lst #(struct:list-val (#(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:diff1-cont
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:try-cont
               x
               #(struct:const-exp -1)
               ((lst
                 #(struct:list-val
                   (#(struct:num-val 2) #(struct:num-val 3))))
                (inner2
                  lst
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:null?-unop)
                      #(struct:var-exp lst))
                    #(struct:raise-exp #(struct:const-exp 99))
                    #(struct:if-exp
                      #(struct:unop-exp
                        #(struct:zero?-unop)
                        #(struct:diff-exp
                          #(struct:unop-exp
                            #(struct:car-unop)
                            #(struct:var-exp lst))
                          #(struct:var-exp n)))
                      #(struct:const-exp 0)
                      #(struct:diff-exp
                        #(struct:call-exp
                          #(struct:var-exp inner2)
                          #(struct:unop-exp
                            #(struct:cdr-unop)
                            #(struct:var-exp lst)))
                        #(struct:const-exp -1)))))
                (n #(struct:num-val 5))
                (i #(struct:num-val 1))
                (v #(struct:num-val 5))
                (x #(struct:num-val 10)))
               #(struct:end-cont))))))))
|(apply-cont
   #(struct:unop-arg-cont
     #(struct:car-unop)
     #(struct:diff1-cont
       #(struct:var-exp n)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:unop-arg-cont
         #(struct:zero?-unop)
         #(struct:if-test-cont
           #(struct:const-exp 0)
           #(struct:diff-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:unop-exp
                 #(struct:cdr-unop)
                 #(struct:var-exp lst)))
             #(struct:const-exp -1))
           ((lst #(struct:list-val (#(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:diff1-cont
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:try-cont
               x
               #(struct:const-exp -1)
               ((lst
                 #(struct:list-val
                   (#(struct:num-val 2) #(struct:num-val 3))))
                (inner2
                  lst
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:null?-unop)
                      #(struct:var-exp lst))
                    #(struct:raise-exp #(struct:const-exp 99))
                    #(struct:if-exp
                      #(struct:unop-exp
                        #(struct:zero?-unop)
                        #(struct:diff-exp
                          #(struct:unop-exp
                            #(struct:car-unop)
                            #(struct:var-exp lst))
                          #(struct:var-exp n)))
                      #(struct:const-exp 0)
                      #(struct:diff-exp
                        #(struct:call-exp
                          #(struct:var-exp inner2)
                          #(struct:unop-exp
                            #(struct:cdr-unop)
                            #(struct:var-exp lst)))
                        #(struct:const-exp -1)))))
                (n #(struct:num-val 5))
                (i #(struct:num-val 1))
                (v #(struct:num-val 5))
                (x #(struct:num-val 10)))
               #(struct:end-cont)))))))
   #(struct:list-val (#(struct:num-val 3))))
|(apply-cont
   #(struct:diff1-cont
     #(struct:var-exp n)
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:unop-arg-cont
       #(struct:zero?-unop)
       #(struct:if-test-cont
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1))
         ((lst #(struct:list-val (#(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:diff1-cont
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont))))))
   #(struct:num-val 3))
|(value-of/k
   #(struct:var-exp n)
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff2-cont
     #(struct:num-val 3)
     #(struct:unop-arg-cont
       #(struct:zero?-unop)
       #(struct:if-test-cont
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1))
         ((lst #(struct:list-val (#(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:diff1-cont
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont)))))))
|(apply-cont
   #(struct:diff2-cont
     #(struct:num-val 3)
     #(struct:unop-arg-cont
       #(struct:zero?-unop)
       #(struct:if-test-cont
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1))
         ((lst #(struct:list-val (#(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:diff1-cont
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont))))))
   #(struct:num-val 5))
|(apply-cont
   #(struct:unop-arg-cont
     #(struct:zero?-unop)
     #(struct:if-test-cont
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1))
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont)))))
   #(struct:num-val -2))
|(apply-cont
   #(struct:if-test-cont
     #(struct:const-exp 0)
     #(struct:diff-exp
       #(struct:call-exp
         #(struct:var-exp inner2)
         #(struct:unop-exp
           #(struct:cdr-unop)
           #(struct:var-exp lst)))
       #(struct:const-exp -1))
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont))))
   #(struct:bool-val #f))
|(value-of/k
   #(struct:diff-exp
     #(struct:call-exp
       #(struct:var-exp inner2)
       #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst)))
     #(struct:const-exp -1))
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff1-cont
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(value-of/k
   #(struct:call-exp
     #(struct:var-exp inner2)
     #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst)))
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff1-cont
     #(struct:const-exp -1)
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(value-of/k
   #(struct:var-exp inner2)
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rator-cont
     #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst))
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont))))))
|(apply-cont
   #(struct:rator-cont
     #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst))
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont)))))
   #(struct:proc-val
     #(struct:procedure
       lst
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:null?-unop)
           #(struct:var-exp lst))
         #(struct:raise-exp #(struct:const-exp 99))
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:zero?-unop)
             #(struct:diff-exp
               #(struct:unop-exp
                 #(struct:car-unop)
                 #(struct:var-exp lst))
               #(struct:var-exp n)))
           #(struct:const-exp 0)
           #(struct:diff-exp
             #(struct:call-exp
               #(struct:var-exp inner2)
               #(struct:unop-exp
                 #(struct:cdr-unop)
                 #(struct:var-exp lst)))
             #(struct:const-exp -1))))
       ((inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10))))))
|(value-of/k
   #(struct:unop-exp #(struct:cdr-unop) #(struct:var-exp lst))
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         ((inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont))))))
|(value-of/k
   #(struct:var-exp lst)
   ((lst #(struct:list-val (#(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:unop-arg-cont
     #(struct:cdr-unop)
     #(struct:rand-cont
       #(struct:proc-val
         #(struct:procedure
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           ((inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst #(struct:list-val (#(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:diff1-cont
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont)))))))
|(apply-cont
   #(struct:unop-arg-cont
     #(struct:cdr-unop)
     #(struct:rand-cont
       #(struct:proc-val
         #(struct:procedure
           lst
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:null?-unop)
               #(struct:var-exp lst))
             #(struct:raise-exp #(struct:const-exp 99))
             #(struct:if-exp
               #(struct:unop-exp
                 #(struct:zero?-unop)
                 #(struct:diff-exp
                   #(struct:unop-exp
                     #(struct:car-unop)
                     #(struct:var-exp lst))
                   #(struct:var-exp n)))
               #(struct:const-exp 0)
               #(struct:diff-exp
                 #(struct:call-exp
                   #(struct:var-exp inner2)
                   #(struct:unop-exp
                     #(struct:cdr-unop)
                     #(struct:var-exp lst)))
                 #(struct:const-exp -1))))
           ((inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst #(struct:list-val (#(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:diff1-cont
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont))))))
   #(struct:list-val (#(struct:num-val 3))))
|(apply-cont
   #(struct:rand-cont
     #(struct:proc-val
       #(struct:procedure
         lst
         #(struct:if-exp
           #(struct:unop-exp
             #(struct:null?-unop)
             #(struct:var-exp lst))
           #(struct:raise-exp #(struct:const-exp 99))
           #(struct:if-exp
             #(struct:unop-exp
               #(struct:zero?-unop)
               #(struct:diff-exp
                 #(struct:unop-exp
                   #(struct:car-unop)
                   #(struct:var-exp lst))
                 #(struct:var-exp n)))
             #(struct:const-exp 0)
             #(struct:diff-exp
               #(struct:call-exp
                 #(struct:var-exp inner2)
                 #(struct:unop-exp
                   #(struct:cdr-unop)
                   #(struct:var-exp lst)))
               #(struct:const-exp -1))))
         ((inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont)))))
   #(struct:list-val ()))
|(value-of/k
   #(struct:if-exp
     #(struct:unop-exp #(struct:null?-unop) #(struct:var-exp lst))
     #(struct:raise-exp #(struct:const-exp 99))
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:zero?-unop)
         #(struct:diff-exp
           #(struct:unop-exp
             #(struct:car-unop)
             #(struct:var-exp lst))
           #(struct:var-exp n)))
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1))))
   ((lst #(struct:list-val ()))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff1-cont
     #(struct:const-exp -1)
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(value-of/k
   #(struct:unop-exp #(struct:null?-unop) #(struct:var-exp lst))
   ((lst #(struct:list-val ()))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:if-test-cont
     #(struct:raise-exp #(struct:const-exp 99))
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:zero?-unop)
         #(struct:diff-exp
           #(struct:unop-exp
             #(struct:car-unop)
             #(struct:var-exp lst))
           #(struct:var-exp n)))
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1)))
     ((lst #(struct:list-val ()))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont))))))
|(value-of/k
   #(struct:var-exp lst)
   ((lst #(struct:list-val ()))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:unop-arg-cont
     #(struct:null?-unop)
     #(struct:if-test-cont
       #(struct:raise-exp #(struct:const-exp 99))
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:zero?-unop)
           #(struct:diff-exp
             #(struct:unop-exp
               #(struct:car-unop)
               #(struct:var-exp lst))
             #(struct:var-exp n)))
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1)))
       ((lst #(struct:list-val ()))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst #(struct:list-val (#(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:diff1-cont
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont)))))))
|(apply-cont
   #(struct:unop-arg-cont
     #(struct:null?-unop)
     #(struct:if-test-cont
       #(struct:raise-exp #(struct:const-exp 99))
       #(struct:if-exp
         #(struct:unop-exp
           #(struct:zero?-unop)
           #(struct:diff-exp
             #(struct:unop-exp
               #(struct:car-unop)
               #(struct:var-exp lst))
             #(struct:var-exp n)))
         #(struct:const-exp 0)
         #(struct:diff-exp
           #(struct:call-exp
             #(struct:var-exp inner2)
             #(struct:unop-exp
               #(struct:cdr-unop)
               #(struct:var-exp lst)))
           #(struct:const-exp -1)))
       ((lst #(struct:list-val ()))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst #(struct:list-val (#(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:diff1-cont
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:try-cont
             x
             #(struct:const-exp -1)
             ((lst
               #(struct:list-val
                 (#(struct:num-val 2) #(struct:num-val 3))))
              (inner2
                lst
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:null?-unop)
                    #(struct:var-exp lst))
                  #(struct:raise-exp #(struct:const-exp 99))
                  #(struct:if-exp
                    #(struct:unop-exp
                      #(struct:zero?-unop)
                      #(struct:diff-exp
                        #(struct:unop-exp
                          #(struct:car-unop)
                          #(struct:var-exp lst))
                        #(struct:var-exp n)))
                    #(struct:const-exp 0)
                    #(struct:diff-exp
                      #(struct:call-exp
                        #(struct:var-exp inner2)
                        #(struct:unop-exp
                          #(struct:cdr-unop)
                          #(struct:var-exp lst)))
                      #(struct:const-exp -1)))))
              (n #(struct:num-val 5))
              (i #(struct:num-val 1))
              (v #(struct:num-val 5))
              (x #(struct:num-val 10)))
             #(struct:end-cont))))))
   #(struct:list-val ()))
|(apply-cont
   #(struct:if-test-cont
     #(struct:raise-exp #(struct:const-exp 99))
     #(struct:if-exp
       #(struct:unop-exp
         #(struct:zero?-unop)
         #(struct:diff-exp
           #(struct:unop-exp
             #(struct:car-unop)
             #(struct:var-exp lst))
           #(struct:var-exp n)))
       #(struct:const-exp 0)
       #(struct:diff-exp
         #(struct:call-exp
           #(struct:var-exp inner2)
           #(struct:unop-exp
             #(struct:cdr-unop)
             #(struct:var-exp lst)))
         #(struct:const-exp -1)))
     ((lst #(struct:list-val ()))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont)))))
   #(struct:bool-val #t))
|(value-of/k
   #(struct:raise-exp #(struct:const-exp 99))
   ((lst #(struct:list-val ()))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:diff1-cont
     #(struct:const-exp -1)
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(value-of/k
   #(struct:const-exp 99)
   ((lst #(struct:list-val ()))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:raise1-cont
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont))))))
|(apply-cont
   #(struct:raise1-cont
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst #(struct:list-val (#(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:diff1-cont
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:try-cont
           x
           #(struct:const-exp -1)
           ((lst
             #(struct:list-val
               (#(struct:num-val 2) #(struct:num-val 3))))
            (inner2
              lst
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:null?-unop)
                  #(struct:var-exp lst))
                #(struct:raise-exp #(struct:const-exp 99))
                #(struct:if-exp
                  #(struct:unop-exp
                    #(struct:zero?-unop)
                    #(struct:diff-exp
                      #(struct:unop-exp
                        #(struct:car-unop)
                        #(struct:var-exp lst))
                      #(struct:var-exp n)))
                  #(struct:const-exp 0)
                  #(struct:diff-exp
                    #(struct:call-exp
                      #(struct:var-exp inner2)
                      #(struct:unop-exp
                        #(struct:cdr-unop)
                        #(struct:var-exp lst)))
                    #(struct:const-exp -1)))))
            (n #(struct:num-val 5))
            (i #(struct:num-val 1))
            (v #(struct:num-val 5))
            (x #(struct:num-val 10)))
           #(struct:end-cont)))))
   #(struct:num-val 99))
|(apply-handler
   #(struct:num-val 99)
   #(struct:diff1-cont
     #(struct:const-exp -1)
     ((lst #(struct:list-val (#(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:diff1-cont
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:try-cont
         x
         #(struct:const-exp -1)
         ((lst
           #(struct:list-val
             (#(struct:num-val 2) #(struct:num-val 3))))
          (inner2
            lst
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:null?-unop)
                #(struct:var-exp lst))
              #(struct:raise-exp #(struct:const-exp 99))
              #(struct:if-exp
                #(struct:unop-exp
                  #(struct:zero?-unop)
                  #(struct:diff-exp
                    #(struct:unop-exp
                      #(struct:car-unop)
                      #(struct:var-exp lst))
                    #(struct:var-exp n)))
                #(struct:const-exp 0)
                #(struct:diff-exp
                  #(struct:call-exp
                    #(struct:var-exp inner2)
                    #(struct:unop-exp
                      #(struct:cdr-unop)
                      #(struct:var-exp lst)))
                  #(struct:const-exp -1)))))
          (n #(struct:num-val 5))
          (i #(struct:num-val 1))
          (v #(struct:num-val 5))
          (x #(struct:num-val 10)))
         #(struct:end-cont)))))
|(apply-handler
   #(struct:num-val 99)
   #(struct:diff1-cont
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:try-cont
       x
       #(struct:const-exp -1)
       ((lst
         #(struct:list-val
           (#(struct:num-val 2) #(struct:num-val 3))))
        (inner2
          lst
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:null?-unop)
              #(struct:var-exp lst))
            #(struct:raise-exp #(struct:const-exp 99))
            #(struct:if-exp
              #(struct:unop-exp
                #(struct:zero?-unop)
                #(struct:diff-exp
                  #(struct:unop-exp
                    #(struct:car-unop)
                    #(struct:var-exp lst))
                  #(struct:var-exp n)))
              #(struct:const-exp 0)
              #(struct:diff-exp
                #(struct:call-exp
                  #(struct:var-exp inner2)
                  #(struct:unop-exp
                    #(struct:cdr-unop)
                    #(struct:var-exp lst)))
                #(struct:const-exp -1)))))
        (n #(struct:num-val 5))
        (i #(struct:num-val 1))
        (v #(struct:num-val 5))
        (x #(struct:num-val 10)))
       #(struct:end-cont))))
|(apply-handler
   #(struct:num-val 99)
   #(struct:try-cont
     x
     #(struct:const-exp -1)
     ((lst
       #(struct:list-val
         (#(struct:num-val 2) #(struct:num-val 3))))
      (inner2
        lst
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:null?-unop)
            #(struct:var-exp lst))
          #(struct:raise-exp #(struct:const-exp 99))
          #(struct:if-exp
            #(struct:unop-exp
              #(struct:zero?-unop)
              #(struct:diff-exp
                #(struct:unop-exp
                  #(struct:car-unop)
                  #(struct:var-exp lst))
                #(struct:var-exp n)))
            #(struct:const-exp 0)
            #(struct:diff-exp
              #(struct:call-exp
                #(struct:var-exp inner2)
                #(struct:unop-exp
                  #(struct:cdr-unop)
                  #(struct:var-exp lst)))
              #(struct:const-exp -1)))))
      (n #(struct:num-val 5))
      (i #(struct:num-val 1))
      (v #(struct:num-val 5))
      (x #(struct:num-val 10)))
     #(struct:end-cont)))
|(value-of/k
   #(struct:const-exp -1)
   ((x #(struct:num-val 99))
    (lst
     #(struct:list-val (#(struct:num-val 2) #(struct:num-val 3))))
    (inner2
      lst
      #(struct:if-exp
        #(struct:unop-exp
          #(struct:null?-unop)
          #(struct:var-exp lst))
        #(struct:raise-exp #(struct:const-exp 99))
        #(struct:if-exp
          #(struct:unop-exp
            #(struct:zero?-unop)
            #(struct:diff-exp
              #(struct:unop-exp
                #(struct:car-unop)
                #(struct:var-exp lst))
              #(struct:var-exp n)))
          #(struct:const-exp 0)
          #(struct:diff-exp
            #(struct:call-exp
              #(struct:var-exp inner2)
              #(struct:unop-exp
                #(struct:cdr-unop)
                #(struct:var-exp lst)))
            #(struct:const-exp -1)))))
    (n #(struct:num-val 5))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))
   #(struct:end-cont))
|(apply-cont #(struct:end-cont) #(struct:num-val -1))
|#(struct:num-val -1)
> 