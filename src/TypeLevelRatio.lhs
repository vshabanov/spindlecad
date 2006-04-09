> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level ratios.

> module TypeLevelRatio where

The following mathematical definition is used for rational fractions:

    data Q = Q Z Peano

Remark that rational fraction set is mathematically defined as set
of equivalent pairs: (m,n)~(m',n') <=> mn'=m'n.

Equivalent a b True  <=> Reduce a =  Reduce b
Equivalent a b False <=> Reduce a /= Reduce b
??? is the equivalent really necessary, there is some troubles with Bool (?)
reduce here must work only with ratios. mega reduce (and +. -. *. /. ???) goes to separate module called ......

Reduce (Q m n) = Reduce (Q (m / gcd m n) (n / gcd m n))
Reduce (Q Zero _) = Q Zero One

----
...these are for minimal representation (in separate file, with
separate equivalent...
Since the main goal of type level arithmetic is to provide
type checked calculations (especially physical dimenstions handling)
we introduce Equivalent and Reduce type classes. They allow us
to work with different types which are actually equivalent.

Equivalent is utility which as the name implies helps us
to determine - is two types numerically equivalent.

Reduce (Q m n) = Reduce (Q (m / gcd m n) (n / gcd m n))
Reduce (Q Zero _) = Zero
Reduce (Q m One) = Reduce m
Reduce (Q (Pos m) n) = Q m n
Reduce (Pos n) = n
Reduce default = default
-------

For details of implementing function level code at type level see the

> import TypeLevelInteger

