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

Reduce (Q m n) = Q (m / gcd m n) (n / gcd m n)
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

> import Data.Ratio -- for use in asRational

Rational fractions data type
    data Q = Q Z Peano

> data (Z m, Peano n) => Q m n = Q m n  deriving (Eq, Ord, Show)

    qplus :: Q -> Q -> Q
    qplus (Q m1 n1) (Q m2 n2) = reduce (Q (m1*n2 + m2*n1) (n1*n2))

> class QPlus a b c | a b -> c where
>     qplus :: a -> b -> c

> instance (ZMultiply m1 (Pos n2) ma, ZMultiply m2 (Pos n1) mb, ZPlus ma mb m,
>           PeanoMultiply n1 n2 n,
>           QReduce (Q m n) (Q mr nr)) => QPlus (Q m1 n1) (Q m2 n2) (Q mr nr) where
>     qplus (Q m1 n1) (Q m2 n2) = qreduce $
>         Q ((m1 `zmultiply` Pos n2) `zplus` (m2 `zmultiply` Pos n1))
>           (n1 `peanoMultiply` n2)

    qnegate :: Q -> Q
    qnegate (Q m n) = Q (znegate m) n

> class QNegate a b | a -> b where
>     qnegate :: a -> b

> instance (Peano n, ZNegate m1 m2) => QNegate (Q m1 n) (Q m2 n) where
>     qnegate (Q m n) = Q (znegate m) n

    qminus :: Q -> Q -> Q
    qminus a b = qplus a (qnegate b)

> class QMinus a b c | a b -> c where
>     qminus :: a -> b -> c

> instance (QNegate b b1, QPlus a b1 c) => QMinus a b c where
>     qminus a b = qplus a (qnegate b)

    qmultiply :: Q -> Q -> Q
    qmultiply (Q m1 n1) (Q m2 n2) = Q (m1*m2) (n1*n2)

> class QMultiply a b c | a b -> c where
>     qmultiply :: a -> b -> c

> instance (ZMultiply m1 m2 m, PeanoMultiply n1 n2 n,
>           QReduce (Q m n) (Q mr nr))
>     => QMultiply (Q m1 n1) (Q m2 n2) (Q mr nr) where
>     qmultiply (Q m1 n1) (Q m2 n2) = qreduce $
>         Q (m1 `zmultiply` m2) (n1 `peanoMultiply` n2)

    qinverse :: Q -> Q
    qinverse (Q (Pos m) n) = Q (Pos n) m
    qinverse (Q (Neg m) n) = Q (Neg n) m
    -- qinverse (Q Zero _) is undefined

> class QInverse a b | a -> b where
>     qinverse :: a -> b

> instance (Peano m, Peano n) => QInverse (Q (Pos m) n) (Q (Pos n) m) where
>     qinverse (Q (Pos m) n) = Q (Pos n) m
> instance (Peano m, Peano n) => QInverse (Q (Neg m) n) (Q (Neg n) m) where
>     qinverse (Q (Neg m) n) = Q (Neg n) m

    qdivide :: Q -> Q -> Q
    qdivide a b = qmultiply a (qinverse b)

> class QDivide a b c | a b -> c where
>     qdivide :: a -> b -> c

> instance (QInverse b b1, QMultiply a b1 c) => QDivide a b c where
>     qdivide a b = qmultiply a (qinverse b)

    qreduce :: Q -> Q
    qreduce (Q Zero One) = Q Zero One
    qreduce (Q Zero (Succ _)) = Q Zero One
    qreduce (Q (Pos m) n) = Q ((Pos m) / gcd m n) (n / gcd m n)
    qreduce (Q (Neg m) n) = Q ((Neg m) / gcd m n) (n / gcd m n)
    -- somewhat duplicate code is made to exclude overlapping instances

The code below is unmaintable - why Haskell can't deduce all these BlaBlaBla' ?

> class QReduce a b | a -> b where
>     qreduce :: a -> b

> instance QReduce (Q Zero One) (Q Zero One) where
>     qreduce (Q Zero One) = Q Zero One
> instance Peano n => QReduce (Q Zero (Succ n)) (Q Zero One) where
>     qreduce (Q Zero (Succ _)) = Q Zero One
> instance (Peano m, Peano n, Peano gcd,
>           ZGcd' (Pos m) (Pos n) gcd, 
>           PeanoDiv m gcd m1,
>           PeanoDiv n gcd (Pos n1), Peano n1,
>           ZGcd' (Pos n) c gcd, PeanoRem' n One m n c
>          ) => QReduce (Q (Pos m) n) (Q m1 n1) where
>     qreduce (Q (Pos m) n) = Q ((m `peanoDiv` gcd)) n1
>         where gcd = zgcd' (Pos m) (Pos n)
>               Pos n1 = n `peanoDiv` gcd
> instance (Peano m, Peano n, Peano gcd,
>           ZGcd' (Pos m) (Pos n) gcd, 
>           PeanoDiv m gcd (Pos m1), Peano m1,
>           PeanoDiv n gcd (Pos n1), Peano n1,
>           ZGcd' (Pos n) c gcd, PeanoRem' n One m n c
>          ) => QReduce (Q (Neg m) n) (Q (Neg m1) n1) where
>     qreduce (Q (Neg m) n) = Q (Neg m1) n1
>         where gcd = zgcd' (Pos m) (Pos n)
>               Pos n1 = n `peanoDiv` gcd
>               Pos m1 = m `peanoDiv` gcd


At the end we define utility function asRational which return Haskell Rational
representation for type level fraction.

Remark that asRational returns the simples (reduced) representation of
type level ratio and therefore may not be equal to actual value of ratio.

> class AsRational a where
>     asRational :: a -> Rational

> instance (Z a, Peano b, AsInteger a, AsInteger b) => AsRational (Q a b) where
>     asRational (Q a b) = asInteger a % asInteger b

