This module contains the exact number data type and operations on it.

> module ExactNumber where

We use exact real arithmetic library from
http://www.cs.man.ac.uk/arch/dlester/exact.html
For more information about libraries for exact arithmetic see:
http://www.haskell.org/haskellwiki/Exact_real_arithmetic

> import qualified ThirdParty.Era as Era

For finite representation of number we keep CASExpr which
can be used to calculate number from scratch.
Finite representation is useful for reading
or writing numbers, so that precision is not loosed.

> import CASExpr

The exact number is built from integers or rationals, or
from some predefined constants like pi.

> import Data.Ratio

The type of exact number itself is a following sum type:

> type EN = Integer Integer
>         | Rational Rational
>         | ExactReal CASExpr CR
>         | NegInfinity
>         | Infinity
>         | NaN
>           deriving (Show)

TODO:
 - add type classes Num, Rational, Floating, Eq, Ord...
   type classes must handle erroneous cases like (sqrt -1, log 0, 1/0,
   0/0, etc.) converting numbers to NegInfinity, Infinity, NaN
 - replace Integer, Rational, Double in CASExpr to single Number EN
 - Funcall in CASExpr must have not string but sum type parameter.
 - make physical dimension value parametrized with number type and
   make EN the default type for physical values
