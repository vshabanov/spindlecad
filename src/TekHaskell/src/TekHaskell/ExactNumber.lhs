--
--  Copyright (C) 2006 Vladimir Shabanov
--
--  This file is part of the TekHaskell.
--
--  The TekHaskell is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  The TekHaskell is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public
--  License along with the TekHaskell; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

This module contains the exact number data type and operations on it.

> module TekHaskell.ExactNumber where

> import qualified TekHaskell.ThirdParty.Era

For the moment this module is only abstracts user from underlying
exact number arithmetic library.

> type ExactNumber = TekHaskell.ThirdParty.Era.CR

WARNING!
DON'T SEE BELOW. THESE THOUGHTS ARE VERY RAW.

After some thoughts I decided that CASExpr and exact numbers
must be completely separated. CASExpr must only contatin Integer
and Rational data types (which are already precise) and provide
`eval` function which can be used to get numerical result of CASExpr.
We can use `eval expr :: Double` to get doubles, or `eval expr :: Era.CR`
to get precise number using Era library (and of course we can use any
other precise numbers library that exports Haskell Num interface).

We have some "overcalculate" problem with this approach. The CASExpr
which contain already calculated exact numbers (with track of CASExpr-s
that can be used for passing to maxima or saving/loading) may be more
performance wise. But its a mess and completely unnecessary for now.

Also, exact real number can contain part evaluated until some precision
and some *finite* data of how to generate the rest. If so we can also
save evaluated part of real numbers and considerably reduce further evaluation
times.

We will return here when start work with contraint database.
(CDB will be used for saving/restoring our calculations, drawings,
tables, guis and so on)

--- CUT ---

We use exact real arithmetic library from
http://www.cs.man.ac.uk/arch/dlester/exact.html
For more information about libraries for exact arithmetic see:
http://www.haskell.org/haskellwiki/Exact_real_arithmetic

-- > import qualified ThirdParty.Era as Era

For finite representation of number we keep CASExpr which
can be used to calculate number from scratch.
Finite representation is useful for reading
or writing numbers, so that precision is not loosed.

-- > import CASExpr

The exact number is built from integers or rationals, or
from some predefined constants like pi.

-- > import Data.Ratio

The type of exact number itself is a following sum type:

-- > data EN = Integer Integer
-- >         | Rational Rational
-- >         | ExactReal CASExpr Era.CR
-- >         | NegInfinity
-- >         | Infinity
-- >         | NaN
-- >           deriving (Show)

TODO:
 - exact computations are too long even for simple numbers
   (see some benchmarks at the end of file)
   We need to find most fastest library (it may be C/C++ one)
   and benchmark it in real applications.
   In general we need CASExpr to contain a way to calculate exact number
   but we need some way to calculate numbers fast. I mean separate functions
   like evalExact & evalDoublePrecision. And of cource eval using CAS
   (this may be more precise than evalDoublePrecision, especially if we use
    fixed precision reals, but more slower than evalDoublePrecision, and
    far more faster than evalExact).
   Also we will need some way to make pluggable exact numbers library
   (since we can switch from Era in future). The simplest one is to implement
    something like class ExactNumberLib (fromInt, fromRat, constPi, constE)
    and implement Num, Float and other classes for libs. After this we can
    simply parametrize EN with concrete lib (or substitute Era.CR with something
    other)
 - make EN abstract type with utilities.
   getInteger :: EN -> Maybe Integer;  getRational :: EN -> Maybe Rational;
   and getCASExpr :: EN -> CASExpr

previous thoughts:
 - add type classes Num, Rational, Floating, Eq, Ord...
   type classes must handle erroneous cases like (sqrt -1, log 0, 1/0,
   0/0, etc.) converting numbers to NegInfinity, Infinity, NaN
 - replace Integer, Rational, Double in CASExpr to single Number EN
 - Funcall in CASExpr must have not string but sum type parameter.
 - make physical dimension value parametrized with number type and
   make EN the default type for physical values

--------------------------------------------------------------------------------
Some time tests for Era and ICReals:

Era

*ThirdParty.Era> take 5 $ iterate (\x -> x + sin x) (1::CR)
[1.0000000000000000000000000000000000000000,1.8414709848078965066525023216302989
996226,2.8050617093497299136094750235092172055890,3.1352763328997160003524035699
574829475624,3.1415926115906531496011613183596168589038]
(13.95 secs, 1099692596 bytes)

ICReals

*Lazy> map (flip eshow 136) $ take 5 $ iterate (\x -> ExpT tadd 1 x (esin x)) (E
xpV (1,1))
["1","0.18414709848078965066525023216302989996225e1","0.280506170934972991360947
50235092172055891e1","0.3135276332899716000352403569957482947562e1","0.314159261
1590653149601161318359616858903e1"]
(-240.82 secs, -841190816 bytes)

I've tried `take 10` instead of `take 5` and get only 8 numbers for Era in two
hours.

