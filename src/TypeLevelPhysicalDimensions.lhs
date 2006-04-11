> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level representation of physical dimenstions.

> module TypeLevelPhysicalDimensions where

The dimension is represented as tuple containing powers of all basic SI units.
Powers are type level numbers from

> import TypeLevelInteger
> import TypeLevelRatio

Definitions of the Seven Basic SI Units:

meter [m]
    The meter is the basic unit of length. It is the distance light travels,
    in a vacuum, in 1/299,792,458th of a second. 
kilogram [kg]
    The kilogram is the basic unit of mass. It is the mass of an international
    prototype in the form of a platinum-iridium cylinder kept at Sevres
    in France. It is now the only basic unit still defined in terms of a
    material object, and also the only one with a prefix[kilo] already in place.
second [s]
    The second is the basic unit of time. It is the length of time taken
    for 9,192,631,770 periods of vibration of the caesium-133 atom to occur. 
ampere [A]
    The ampere is the basic unit of electric current. It is that current which
    produces a specified force between two parallel wires which are 1 meter
    apart in a vacuum.
    It is named after the French physicist Andre Ampere (1775-1836). 
kelvin [K]
    The kelvin is the basic unit of temperature. It is 1/273.16th of the
    thermodynamic temperature of the triple point of water.
    It is named after the Scottish mathematician and physicist
    William Thomson 1st Lord Kelvin (1824-1907).
mole [mol]
    The mole is the basic unit of substance. It is the amount of substance
    that contains as many elementary units as there are
    atoms in 0.012 kg of carbon-12. 
candela [cd]
    The candela is the basic unit of luminous intensity. It is the intensity
    of a source of light of a specified frequency, which gives a specified
    amount of power in a given direction. 


Type level dimension definition.
All type parameters are the simplest representation of type level numbers
corresponding to power of element. The point is that simplest representation
is also unique one.
The simplest representation is obtained using 'simplify' after any
operation on dimensions (multiplication, division, exponentiation). 

> data Dimension m kg s a k mol cd = Dimension m kg s a k mol cd
>                                    deriving (Eq, Show, Read)


> type Meter    = Dimension One  Zero Zero Zero Zero Zero Zero
> type Kilogram = Dimension Zero One  Zero Zero Zero Zero Zero
> type Second   = Dimension Zero Zero One  Zero Zero Zero Zero
> type Ampere   = Dimension Zero Zero Zero One  Zero Zero Zero
> type Kelvin   = Dimension Zero Zero Zero Zero One  Zero Zero
> type Mole     = Dimension Zero Zero Zero Zero Zero One  Zero
> type Candela  = Dimension Zero Zero Zero Zero Zero Zero One  


> dimensionMultiply a b =
>     dimensionSimplify
>     (Dimension
>      (am `qplus` bm) (akg `qplus` bkg) (as `qplus` bs)
>      (aa `qplus` ba) (ak `qplus` bk) (amol `qplus` bmol) (acd `qplus` bcd))
>         where Dimension am akg as aa ak amol acd = dimensionAsQ a
>               Dimension bm bkg bs ba bk bmol bcd = dimensionAsQ b

> dimensionDivide a b =
>     dimensionSimplify
>     (Dimension
>      (am `qminus` bm) (akg `qminus` bkg) (as `qminus` bs)
>      (aa `qminus` ba) (ak `qminus` bk) (amol `qminus` bmol) (acd `qminus` bcd))
>         where Dimension am akg as aa ak amol acd = dimensionAsQ a
>               Dimension bm bkg bs ba bk bmol bcd = dimensionAsQ b


Some utility functions used to translate basic units powers in dimenstion
to fractions and simplify them back.

> dimensionAsQ (Dimension m kg s a k mol cd) =
>     Dimension (asQ m) (asQ kg) (asQ s) (asQ a) (asQ k) (asQ mol) (asQ cd)

> dimensionSimplify (Dimension m kg s a k mol cd) =
>     Dimension (simplify m) (simplify kg) (simplify s)
>                   (simplify a) (simplify k) (simplify mol) (simplify cd)


To Z converter. Converts any type level number to rational fraction.

    asQ :: _ -> Z
    asQ Zero = Q Zero One
    asQ One = Q (Pos One) One
    asQ (Succ a) = Q (Pos (Succ a)) One
    asQ (Pos a) = Q (Pos a) One
    asQ (Neg a) = Q (Neg a) One
    asQ (Q m n) = qreduce (Q m n)

> class AsQ a b | a -> b where
>     asQ :: a -> b

> instance AsQ Zero (Q Zero One) where
>     asQ Zero = Q Zero One
> instance AsQ One (Q (Pos One) One) where
>     asQ One = Q (Pos One) One
> instance Peano a => AsQ (Succ a) (Q (Pos (Succ a)) One) where
>     asQ (Succ a) = Q (Pos (Succ a)) One
> instance Peano a => AsQ (Pos a) (Q (Pos a) One) where
>     asQ (Pos a) = Q (Pos a) One
> instance Peano a => AsQ (Neg a) (Q (Neg a) One) where
>     asQ (Neg a) = Q (Neg a) One
> instance (Z m, Peano n, QReduce (Q m n) (Q mr nr))
>     => AsQ (Q m n) (Q mr nr) where
>     asQ (Q m n) = qreduce (Q m n)

Type level number simplifier.

    simplify :: _ -> _
    simplify (Q Zero (Succ a)) = Zero
    simplify (Q m One)  = simplify m
    simplify (Q m (Succ a)) = simplifyOne $ qreduce (Q m (Succ a))
    simplify (Pos a) = a
    simplify (Neg a) = Neg a
    simplify (Succ a) = Succ a
    simplify One = One
    simplify Zero = Zero
    simplifyOne (Q m One) = simplify m
    simplifyOne (Q m (Succ a)) = (Q m (Succ a))

> class Simplify a b | a -> b where
>     simplify :: a -> b
> class SimplifyOne a b | a -> b where
>     simplifyOne :: a -> b

> instance Peano a => Simplify (Q Zero (Succ a)) Zero where
>     simplify (Q Zero (Succ a)) = Zero
> instance (Z m, Simplify m mr) => Simplify (Q m One) mr where
>     simplify (Q m One)  = simplify m
> instance (Z m, Peano a, QReduce (Q m (Succ a)) (Q mr nr),
>           SimplifyOne (Q mr nr) r) => Simplify (Q m (Succ a)) r where
>     simplify (Q m (Succ a)) = simplifyOne $ qreduce (Q m (Succ a))
> instance Peano a => Simplify (Pos a) a where
>     simplify (Pos a) = a
> instance Peano a => Simplify (Neg a) (Neg a) where
>     simplify (Neg a) = Neg a
> instance Peano a => Simplify (Succ a) (Succ a) where
>     simplify (Succ a) = Succ a
> instance Simplify One One where
>     simplify One = One
> instance Simplify Zero Zero where
>     simplify Zero = Zero

> instance (Z m, Simplify m mr) => SimplifyOne (Q m One) mr where
>     simplifyOne (Q m One)  = simplify m
> instance (Z m, Peano a) => SimplifyOne (Q m (Succ a)) (Q m (Succ a)) where
>     simplifyOne (Q m (Succ a)) = (Q m (Succ a))
