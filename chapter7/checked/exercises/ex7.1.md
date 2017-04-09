> Exercise 7.1 [*] Below is a list of closed expressions. Consider the value of each
expression. For each value, what type or types does it have? Some of the values
may have no type that is describable in our type language.

```
1. proc (x) -(x,3)
:: int -> int

2. proc (f) proc (x) -((f x), 1)
:: (int -> int) -> (int -> int)

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