--
--  Copyright (C) 2006 Vladimir Shabanov
--
--  This file is part of SpindleCAD.
--
--  SpindleCAD is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  SpindleCAD is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Foobar; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
--

> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level ratios.

> module TypeLevelRatio where

The following mathematical definition is used for rational fractions:

    data Q = Q Z Peano

Remark that rational fraction set is mathematically defined as set
of equivalent pairs: (m,n)~(m',n') <=> mn'=m'n.

For details of implementing function level code at type level see the

> import TypeLevelInteger
> import TypeLevelBoolean

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

Extension to non-zero type level predicate defined in TypeLevelInteger

> instance (Peano n) => NonZero (Q Zero n) BFalse where
>     nonzero (Q Zero n) = BFalse
> instance (Peano m, Peano n) => NonZero (Q (Pos m) n) BTrue where
>     nonzero (Q (Pos m) n) = BTrue
> instance (Peano m, Peano n) => NonZero (Q (Neg m) n) BTrue where
>     nonzero (Q (Neg m) n) = BTrue

Type values

> instance (TypeValue a, TypeValue b, Z a, Peano b) => TypeValue (Q a b) where
>     typeValue = Q (typeValue :: a) (typeValue :: b)


At the end we define utility function asRational which return Haskell Rational
representation for type level fraction.

Remark that asRational returns the simples (reduced) representation of
type level ratio and therefore may not be equal to actual value of ratio.

> class AsRational a where
>     asRational :: a -> Rational

> instance (Z a, Peano b, AsInteger a, AsInteger b) => AsRational (Q a b) where
>     asRational (Q a b) = asInteger a % asInteger b

