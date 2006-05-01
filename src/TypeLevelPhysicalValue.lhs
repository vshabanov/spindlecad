> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level representation of physical values.

> module TypeLevelPhysicalValue where

> import TypeLevelPhysicalDimension
> import TypeLevelInteger -- Zero
> import TypeLevelBoolean -- BTrue, BFalse
> import CASExpr


The phantom type for physical values. It only contain numerical value
but it is type checked at compile time.
Its pretty good to have (Num a) value and get the same behaviour
for values as for ordinary haskell numbers. But its hard to work
with derived units, e.g. newton = kilogram *. meter /. (second *. second)
will have type of Value Double, but primitive units like kilogram
have type of Value Integer. Thats incoherence. It can be solved by
making values of Rational type, but then we can't call (Real a) funcions
like sin and cos without type cast. So, for consistency reasons,
all values are made of Double type.
This is subject to change (all values as Rational). Think
about it after CAS interface & constraint solver are done.

> data Value d = Value CASExpr deriving (Eq, Ord, Show)


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


Operators precedence

> infixl 7  *., /.
> infixl 6  +., -.


Since any haskell digit by default has type (Num a) => a we can't
provide *. or /. operators which will work with digits and values
simultaneously. I.e. we can't get '10 *. kg' to work, only
something like '(10::Integer) *. kg'.
So we create additional pair of operators .* and ./
that receive number as first argument,
so awful '(10::Integer) *. kg /. s' goes into '10 .* kg /. s'.
Here they are:

> infixl 8  .*, ./

> (.*) :: CASExpr -> Value d -> Value d
> a .* Value b = Value (a * b)

> (./) :: (DimensionDivide NonDim d2 d)
>         => CASExpr -> Value d2 -> Value d
> a ./ Value b = Value (a / b)


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
