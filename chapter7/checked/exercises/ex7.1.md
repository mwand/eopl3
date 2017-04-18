> Exercise 7.1 [*] Below is a list of closed expressions. Consider the value of each
expression. For each value, what type or types does it have? Some of the values
may have no type that is describable in our type language.

```
1. proc (x) -(x,3)
:: int -> int

2. proc (f) proc (x) -((f x), 1)
:: (t -> int) -> (t -> int)

3. proc (x) x
:: t -> t

4. proc (x) proc (y) (x y).
:: (t1 -> t2) -> (t1 -> t2)

5. proc (x) (x 3)
:: (int -> t) -> t

6. proc (x) (x x)
:: no-type

7. proc (x) if x then 88 else 99
:: bool -> int

8. proc (x) proc (y) if x then y else 99
:: bool -> (int -> int)

9. (proc (p) if p then 88 else 99
    33)
:: no-type

10. (proc (p) if p then 88 else 99
    proc (z) z)
:: no-type

11. proc (f)
      proc (g)
        proc (p)
          proc (x) if (p (f x)) then (g 1) else -((f x),1)
:: (int -> int) -> (int -> int) -> (int -> bool) -> (int -> int)

12. proc (x)
      proc(p)
        proc (f)
          if (p x) then -(x,1) else (f p)
:: int -> (int -> bool) -> ((int -> bool) -> int)

13. proc (f)
      let d = proc (x)
        proc (z) ((f (x x)) z)
      in proc (n) ((f (d d)) n)
:: no-type
(Y-combinator)
```

> Exercise 7.2 [**] Are there any expressed values that have exactly two types according to definition 7.1.1?

proc (x) x
:: t -> t

> Exercise 7.3 [**] For the language LETREC, is it decidable whether an expressed
value val is of type t?

ex7.1, problem 13, undecidable?

> Exercise 7.4 [*] Using the rules of this section, write derivations, like the one on
page 5, that assign types for proc (x) x and proc (x) proc (y) (x y). Use
the rules to assign at least two types for each of these terms. Do the values of these
expressions have the same types?

```
x ∈ Int
----------------------
proc (x) x ∈ (Int -> Int)

x ∈ Bool
----------------------
proc (x) x ∈ (Bool -> Bool)

x ∈ Bool -> Int  y ∈ Bool
----------------------
proc (x) proc (y) (x y) ∈ Int

x ∈ Int -> Bool  y ∈ Int
----------------------
proc (x) proc (y) (x y) ∈ Bool
```

Answer: Not always.

> Exercise 7.12 [*] Using the methods in this section, derive types for
> each of the expressions in exercise 7.1, or determine that no such type
> exists. As in the other examples of this section, assume there is a ?
> attached to each bound variable.

```
1. proc (x) -(x,3)
:: int -> int

Expression                                Type Variable
-------------------------------------------------------
proc (x) -(x,3)                           t0
-(x,3)                                    t1
x                                         tx

Expression                                Equations
-------------------------------------------------------
proc (x) -(x,3)                           t0 = tx -> t1
-(x,3)                                    t1 = int
                                          tx = int

Equations                                 Substitution
-------------------------------------------------------
t0 = tx -> t1
t1 = int
tx = int

Equations                                 Substitution
-------------------------------------------------------
t1 = int                                  t0 = tx -> t1
tx = int

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> int
tx = int                                  t1 = int 

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = int -> int
                                          t1 = int 
                                          tx = int


2. proc (f) proc (x) -((f x), 1)
:: (tx -> int) -> (tx -> int)

Expression                                Type Variable
-------------------------------------------------------
proc (f) proc (x) -((f x), 1)             t0
proc (x) -((f x), 1)                      t1
-((f x), 1)                               t2
(f x)                                     t3
f                                         tf
x                                         tx

Expression                                Equations
-------------------------------------------------------
proc (f) proc (x) -((f x), 1)             t0 = tf -> t1
proc (x) -((f x), 1)                      t1 = tx -> t2
-((f x), 1)                               t2 = int
                                          t3 = int
(f x)                                     tf = tx -> t3

Equations                                 Substitution
-------------------------------------------------------
t0 = tf -> t1                             
t1 = tx -> t2
t2 = int
t3 = int
tf = tx -> t3

Equations                                 Substitution
-------------------------------------------------------
t1 = tx -> t2                             t0 = tf -> t1                             
t2 = int
t3 = int
tf = tx -> t3

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tf -> (tx -> t2)
t2 = int                                  t1 = tx -> t2 
t3 = int
tf = tx -> t3

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tf -> (tx -> int)
                                          t1 = tx -> int
                                          t2 = int 
tf = tx -> t3                             t3 = int 

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tf -> (tx -> int)
                                          t1 = tx -> int
                                          t2 = int 
tf = tx -> int                            t3 = int 

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = (tx -> int) -> (tx -> int)
                                          t1 = tx -> int
                                          t2 = int 
                                          t3 = int 
                                          tf = tx -> int

3. proc (x) x
:: tx -> tx

Expression                                Type Variable
-------------------------------------------------------
proc (x) x                                t0
x                                         tx

Expression                                Equations
-------------------------------------------------------
proc (x) x                                t0 = tx -> tx

Equations                                 Substitution
-------------------------------------------------------
t0 = tx -> tx

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> tx


4. proc (x) proc (y) (x y).
:: (ty -> t2) -> (ty -> t2)

Expression                                Type Variable
-------------------------------------------------------
proc (x) proc (y) (x y)                   t0
proc (y) (x y)                            t1
(x y)                                     t2
x                                         tx
y                                         ty

Expression                                Equations
-------------------------------------------------------
proc (x) proc (y) (x y)                   t0 = tx -> t1
proc (y) (x y)                            t1 = ty -> t2
(x y)                                     tx = ty -> t2

Equations                                 Substitution
-------------------------------------------------------
t0 = tx -> t1                             
t1 = ty -> t2
tx = ty -> t2

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> t1                             
t1 = ty -> t2
tx = ty -> t2

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> (ty -> t2)
                                          t1 = ty -> t2
tx = ty -> t2

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = (ty -> t2) -> (ty -> t2)
                                          t1 = ty -> t2
                                          tx = ty -> t2

5. proc (x) (x 3)
:: (int -> t) -> t

Expression                                Type Variable
-------------------------------------------------------
proc (x) (x 3)                            t0
(x 3)                                     t1
x                                         tx

Expression                                Equations
-------------------------------------------------------
proc (x) (x 3)                            t0 = tx -> t1
(x 3)                                     tx = int -> t1

Equations                                 Substitution
-------------------------------------------------------
t0 = tx -> t1
tx = int -> t1

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = (int -> t1) -> t1
                                          tx = int -> t1

6. proc (x) (x x)
:: no-type

Expression                                Type Variable
-------------------------------------------------------
proc (x) (x x)                            t0
(x x)                                     t1
x                                         tx

Expression                                Equations
-------------------------------------------------------
proc (x) (x x)                            t0 = tx -> t1
(x x)                                     tx = tx -> t1

Equations                                 Substitution
-------------------------------------------------------
t0 = tx -> t1
tx = tx -> t1

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> t1
tx = tx -> t1

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> t1
                                          tx = tx -> t1

break "the no-occurrence invariant" rule.

7. proc (x) if x then 88 else 99
:: bool -> int

Expression                                Type Variable
-------------------------------------------------------
proc (x) if x then 88 else 99             t0
if x then 88 else 99                      t1
x                                         tx

Expression                                Equations
-------------------------------------------------------
proc (x) if x then 88 else 99             t0 = tx -> t1
if x then 88 else 99                      t1 = int
                                          tx = bool

Equations                                 Substitution
-------------------------------------------------------
t0 = tx -> t1
t1 = int
tx = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> t1
t1 = int
tx = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> int
                                          t1 = int
tx = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = bool -> int
                                          t1 = int
                                          tx = bool

8. proc (x) proc (y) if x then y else 99
:: bool -> (int -> int)

Expression                                Type Variable
-------------------------------------------------------
proc (x) proc (y) if x then y else 99     t0
proc (y) if x then y else 99              t1
if x then y else 99                       t2
x                                         tx
y                                         ty

Expression                                Equations
-------------------------------------------------------
proc (x) proc (y) if x then y else 99     t0 = tx -> t1
proc (y) if x then y else 99              t1 = ty -> t2
if x then y else 99                       t2 = int
                                          tx = bool
                                          ty = int

Equations                                 Substitution
-------------------------------------------------------
t0 = tx -> t1
t1 = ty -> t2
t2 = int
tx = bool
ty = int

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> t1
t1 = ty -> t2
t2 = int
tx = bool
ty = int

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> (ty -> t2)
                                          t1 = ty -> t2
t2 = int
tx = bool
ty = int

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = tx -> (ty -> int)
                                          t1 = ty -> int
                                          t2 = int
tx = bool
ty = int

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = bool -> (ty -> int)
                                          t1 = ty -> int
                                          t2 = int
                                          tx = bool
ty = int

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = bool -> (int -> int)
                                          t1 = int -> int
                                          t2 = int
                                          tx = bool
                                          ty = int

9. (proc (p) if p then 88 else 99
    33)
:: no-type

Expression                                Type Variable
-------------------------------------------------------
(proc (p) if p then 88 else 99            
    33)                                   t0
proc (p) if p then 88 else 99             t1
if p then 88 else 99                      t2
p                                         tp

Expression                                Equations
-------------------------------------------------------
(proc (p) if p then 88 else 99            
    33)                                   t1 = int -> t2
proc (p) if p then 88 else 99             t1 = tp -> t2
if p then 88 else 99                      tp = bool

Equations                                 Substitution
-------------------------------------------------------
t1 = int -> t2                        
t1 = tp -> t2
tp = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t1 = int -> t2                        
t1 = tp -> t2
tp = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t1 = int -> t2
int -> t2 = tp -> t2
tp = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t1 = int -> t2
int -> t2 = tp -> t2
tp = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t1 = int -> t2
int = tp
tp = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t1 = int -> t2
tp = int
tp = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t1 = int -> t2
                                          tp = int
tp = bool

Equations                                 Substitution
-------------------------------------------------------
                                          t1 = int -> t2
                                          tp = int
int = bool

no type.

10. (proc (p) if p then 88 else 99
    proc (z) z)
:: no-type



11. proc (f)
      proc (g)
        proc (p)
          proc (x) if (p (f x)) then (g 1) else -((f x),1)
:: (int -> int) -> (int -> int) -> (int -> bool) -> (int -> int)

12. proc (x)
      proc(p)
        proc (f)
          if (p x) then -(x,1) else (f p)
:: int -> (int -> bool) -> ((int -> bool) -> int)

13. proc (f)
      let d = proc (x)
        proc (z) ((f (x x)) z)
      in proc (n) ((f (d d)) n)
:: no-type
(Y-combinator)
```

> Exercise 7.13 [*] Write down a rule for doing type inference for let expressions. Using your rule, derive types for each of the following expressions, or determine that no such type exists.
> 1. let x = 4 in (x 3)
> 2. let f = proc (z) z in proc (x) -((f x), 1)
> 3. let p = zero?(1) in if p then 88 else 99
> 4. let p = proc (z) z in if p then 88 else 99

;; let var = let-exp in body-exp : t(var) = t(let-exp)


1. let x = 4 in (x 3)

```
Expression                                Type Variable
-------------------------------------------------------
let x = 4 in (x 3)                        t0
(x 3)                                     t1
x                                         tx

Expression                                Equations
-------------------------------------------------------
let x = 4 in (x 3)                        t0 = t1
x = 4                                     tx = int
(x 3)                                     tx = int -> t1

Equations                                 Substitution
-------------------------------------------------------
t0 = t1
tx = int
tx = int -> t1

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = t1
tx = int
tx = int -> t1

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = t1
                                          tx = int
tx = int -> t1

Equations                                 Substitution
-------------------------------------------------------
                                          t0 = t1
                                          tx = int
int = int -> t1
```

Break "the no-occurrence invariant" rule.


> Exercise 7.14 [*] What is wrong with this expression?
> 
```
letrec 
  ? even(odd : ?) = 
    proc (x : ?) 
      if zero?(x) then 1 else (odd -(x,1)) 
in letrec 
  ? odd(x : bool) = 
    if zero?(x) then 0 else ((even odd) -(x,1)) 
  in (odd 13)
```

In first `letrec` we can have equation `t(odd) = int -> int`, but in second `letrec` we have `t(odd) = bool -> int`.
They have no solution.

> Exercise 7.15 [**] Write down a rule for doing type inference for a letrec expression. Your rule should handle multiple 
> declarations in a letrec. Using your rule, derive types for each of the following expressions, or determine that no such type exists:

```

```