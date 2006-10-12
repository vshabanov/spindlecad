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
--  along with SpindleCAD; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
--

> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level representation of physical values.

> module TypeLevelPhysicalValue where

> import TypeLevelPhysicalDimension
> import TypeLevelInteger -- Zero
> import TypeLevelBoolean -- BTrue, BFalse
> import qualified Data.Map as Map
> import CASExpr


The phantom type for physical values. It only contain numerical value
but it is type checked at compile time.

> newtype Value d = Value CASExpr deriving (Eq, Ord, Show, Enum)

> type NondimensionalValue = CASExpr
> type Angle = NondimensionalValue

Basic units

> meter    = Value 1 :: Value Meter
> kilogram = Value 1 :: Value Kilogram 
> second   = Value 1 :: Value Second   
> ampere   = Value 1 :: Value Ampere   
> kelvin   = Value 1 :: Value Kelvin   
> mole     = Value 1 :: Value Mole     
> candela  = Value 1 :: Value Candela  


Unit power prefixes

> power n = ((10 ^^ n :: CASExpr) .*)

> yotta = power $  24  -- Y
> zetta = power $  21  -- Z
> exa   = power $  18  -- E
> peta  = power $  15  -- P
> tera  = power $  12  -- T
> giga  = power $   9  -- G
> mega  = power $   6  -- M
> kilo  = power $   3  -- k
> hecto = power $   2  -- h
> deca  = power $   1  -- da
> deci  = power $  -1  -- d
> centi = power $  -2  -- c
> milli = power $  -3  -- m
> micro = power $  -6  -- mu
> nano  = power $  -9  -- n
> pico  = power $ -12  -- p
> femto = power $ -15  -- f
> atto  = power $ -18  -- a
> zepto = power $ -21  -- z
> yocto = power $ -24  -- y


Arithmetic operators.

Plus.

> ( +. ) :: Value d -> Value d -> Value d
> (Value a) +. (Value b) = Value (a+b)

Minus.

> ( -. ) :: Value d -> Value d -> Value d
> (Value a) -. (Value b) = Value (a-b) 

Multiplication.

> ( *. ) :: (DimensionMultiply d1 d2 d, ValueSimplify (Value d) r,
>            ValueSimplify' (Value d) nd r, Nondimensional d nd, TypeValue d)
>           => Value d1 -> Value d2 -> r
> a *. b = valueSimplify $ mul' a b
>     where mul' :: (DimensionMultiply d1 d2 d)
>                   => Value d1 -> Value d2 -> Value d
>           mul' (Value a) (Value b) = Value (a*b)

Division.

> ( /. ) :: (DimensionDivide d1 d2 d,
>            ValueSimplify (Value d) r,
>            ValueSimplify' (Value d) nd r, Nondimensional d nd, TypeValue d)
>           => Value d1 -> Value d2 -> r
> a /. b = valueSimplify $ mul' a b
>     where mul' :: (DimensionDivide d1 d2 d)
>                   => Value d1 -> Value d2 -> Value d
>           mul' (Value a) (Value b) = Value (a / b)

For work with ordinary number two more operators was introduced.

> (.*) :: CASExpr -> Value d -> Value d
> a .* Value b = Value (a * b)

> (./) :: (DimensionDivide NonDim d2 d)
>         => CASExpr -> Value d2 -> Value d
> a ./ Value b = Value (a / b)

Remark:
Unfortunately we can't make one operator which accept numbers and
physical values at once. Rather, we can do this using type classes,
but then we need to specify number type. Something like "(1::CASExpr) *. meter",
which is not useful.


Operators precedence

> infixl 7  *., /.
> infixl 6  +., -.
> infixl 8  .*, ./

Substitution of symbols found in value.
substitutepv - same as CASExpr.substitute but for physical values

> substitutepv :: Map.Map String CASExpr -> Value a -> Value a
> substitutepv m (Value v) = Value (substitute m v)

simplified interface

> substitutepv' :: [(String, CASExpr)] -> Value a -> Value a
> substitutepv' s = let m = Map.fromList s in
>                   (\ (Value v) -> Value (substitute m v))


Value simplification. It converts nondimensional argument to Double and
return dimensional argument unchanged.

> class ValueSimplify a b | a -> b where
>     valueSimplify :: a -> b

> class ValueSimplify' a b c | a b -> c where
>     valueSimplify' :: a -> b -> c

> instance (TypeValue d, Nondimensional d nd, ValueSimplify' (Value d) nd r)
>     => ValueSimplify (Value d) r where
>     valueSimplify (Value a) =
>          valueSimplify' (Value a :: Value d) (nondimensional (typeValue :: d))

> instance ValueSimplify' (Value d) BTrue CASExpr where
>     valueSimplify' (Value a) BTrue = a
> instance ValueSimplify' (Value d) BFalse (Value d) where
>     valueSimplify' (Value a) BFalse = (Value a)
