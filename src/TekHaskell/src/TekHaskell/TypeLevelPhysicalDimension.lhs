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

Type level representation of physical dimenstions.

> module TypeLevelPhysicalDimension where

The dimension is represented as tuple containing powers of all basic SI units.
Powers are type level numbers from

> import TypeLevelInteger
> import TypeLevelRatio
> import TypeLevelBoolean

Definitions of the Seven Basic SI Units:
(taken from http://www.electro-optical.com/unitconv/unitdict/basic_si.htm)

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

> type NonDim   = Dimension Zero Zero Zero Zero Zero Zero Zero

> type Meter    = Dimension One  Zero Zero Zero Zero Zero Zero
> type Kilogram = Dimension Zero One  Zero Zero Zero Zero Zero
> type Second   = Dimension Zero Zero One  Zero Zero Zero Zero
> type Ampere   = Dimension Zero Zero Zero One  Zero Zero Zero
> type Kelvin   = Dimension Zero Zero Zero Zero One  Zero Zero
> type Mole     = Dimension Zero Zero Zero Zero Zero One  Zero
> type Candela  = Dimension Zero Zero Zero Zero Zero Zero One  

The functions here (and above) can be defined as ordinary Haskell functions
(without class/instance pair) but for use in other classes its better to make
them type class also.

> class DimensionMultiply a b c | a b -> c where
>     dimensionMultiply :: a -> b -> c

> instance (DimensionAsQ (Dimension m1 kg1 s1 a1 k1 mol1 cd1)
>           (Dimension am akg as aa ak amol acd),
>           DimensionAsQ (Dimension m2 kg2 s2 a2 k2 mol2 cd2)
>           (Dimension bm bkg bs ba bk bmol bcd),
>           DimensionSimplify (Dimension m kg s a k mol cd)
>           (Dimension mr kgr sr ar kr molr cdr),
>           QPlus am bm m, QPlus akg bkg kg, QPlus as bs s,
>           QPlus aa ba a, QPlus ak bk k, QPlus amol bmol mol, QPlus acd bcd cd,
>           -- all things below added because of 'could not deduce' error
>           Simplify m mr, Simplify kg kgr, Simplify s sr, Simplify a ar,
>           Simplify k kr, Simplify mol molr, Simplify cd cdr,
>           AsQ m1 am, AsQ kg1 akg, AsQ s1 as, AsQ a1 aa,
>           AsQ k1 ak, AsQ mol1 amol, AsQ cd1 acd,
>           AsQ m2 bm, AsQ kg2 bkg, AsQ s2 bs, AsQ a2 ba,
>           AsQ k2 bk, AsQ mol2 bmol, AsQ cd2 bcd--,
>          )
>     => DimensionMultiply
>            (Dimension m1 kg1 s1 a1 k1 mol1 cd1)
>            (Dimension m2 kg2 s2 a2 k2 mol2 cd2)
>            (Dimension mr kgr sr ar kr molr cdr) where
>     dimensionMultiply a b =
>         dimensionSimplify
>         (Dimension
>          (am `qplus` bm) (akg `qplus` bkg) (as `qplus` bs)
>          (aa `qplus` ba) (ak `qplus` bk) (amol `qplus` bmol) (acd `qplus` bcd))
>             where Dimension am akg as aa ak amol acd = dimensionAsQ a
>                   Dimension bm bkg bs ba bk bmol bcd = dimensionAsQ b

> class DimensionDivide a b c | a b -> c where
>     dimensionDivide :: a -> b -> c

> instance (DimensionAsQ (Dimension m1 kg1 s1 a1 k1 mol1 cd1)
>           (Dimension am akg as aa ak amol acd),
>           DimensionAsQ (Dimension m2 kg2 s2 a2 k2 mol2 cd2)
>           (Dimension bm bkg bs ba bk bmol bcd),
>           DimensionSimplify (Dimension m kg s a k mol cd)
>           (Dimension mr kgr sr ar kr molr cdr),
>           QMinus am bm m, QMinus akg bkg kg, QMinus as bs s,
>           QMinus aa ba a, QMinus ak bk k, QMinus amol bmol mol, QMinus acd bcd cd,
>           -- all things below added because of 'could not deduce' error
>           QPlus am nbm m, QPlus akg nbkg kg, QPlus as nbs s,
>           QPlus aa nba a, QPlus ak nbk k, QPlus amol nbmol mol, QPlus acd nbcd cd,
>           QNegate bm nbm, QNegate bkg nbkg, QNegate bs nbs,
>           QNegate ba nba, QNegate bk nbk, QNegate bmol nbmol, QNegate bcd nbcd,
>           Simplify m mr, Simplify kg kgr, Simplify s sr, Simplify a ar,
>           Simplify k kr, Simplify mol molr, Simplify cd cdr,
>           AsQ m1 am, AsQ kg1 akg, AsQ s1 as, AsQ a1 aa,
>           AsQ k1 ak, AsQ mol1 amol, AsQ cd1 acd,
>           AsQ m2 bm, AsQ kg2 bkg, AsQ s2 bs, AsQ a2 ba,
>           AsQ k2 bk, AsQ mol2 bmol, AsQ cd2 bcd--,
>          )
>     => DimensionDivide
>            (Dimension m1 kg1 s1 a1 k1 mol1 cd1)
>            (Dimension m2 kg2 s2 a2 k2 mol2 cd2)
>            (Dimension mr kgr sr ar kr molr cdr) where
>     dimensionDivide a b =
>         dimensionSimplify
>         (Dimension
>          (am `qminus` bm) (akg `qminus` bkg) (as `qminus` bs)
>          (aa `qminus` ba) (ak `qminus` bk) (amol `qminus` bmol) (acd `qminus` bcd))
>             where Dimension am akg as aa ak amol acd = dimensionAsQ a
>                   Dimension bm bkg bs ba bk bmol bcd = dimensionAsQ b


Some utility functions used to translate basic units powers in dimenstion
to fractions and simplify them back.

> class DimensionAsQ a b | a -> b where
>     dimensionAsQ :: a -> b

> instance (AsQ m m1, AsQ kg kg1, AsQ s s1, AsQ a a1,
>           AsQ k k1, AsQ mol mol1, AsQ cd cd1)
>     => DimensionAsQ (Dimension m kg s a k mol cd)
>            (Dimension m1 kg1 s1 a1 k1 mol1 cd1) where
>     dimensionAsQ (Dimension m kg s a k mol cd) =
>         Dimension (asQ m) (asQ kg) (asQ s) (asQ a) (asQ k) (asQ mol) (asQ cd)

> class DimensionSimplify a b | a -> b where
>     dimensionSimplify :: a -> b

> instance (Simplify m m1, Simplify kg kg1, Simplify s s1, Simplify a a1,
>           Simplify k k1, Simplify mol mol1, Simplify cd cd1)
>     => DimensionSimplify (Dimension m kg s a k mol cd)
>            (Dimension m1 kg1 s1 a1 k1 mol1 cd1) where
>     dimensionSimplify (Dimension m kg s a k mol cd) =
>         Dimension (simplify m) (simplify kg) (simplify s)
>                       (simplify a) (simplify k) (simplify mol) (simplify cd)


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

Non-dimensional type level predicate

> class (Boolean b) => Nondimensional a b | a -> b where
>     nondimensional :: a -> b

> instance (NonZero m mz, NonZero kg kgz, NonZero s sz, NonZero a az,
>           NonZero k kz, NonZero mol molz, NonZero cd cdz,
>           BOr mz kgz r1, BOr r1 sz r2, BOr r2 az r3,  BOr r3 kz r4,
>           BOr r4 molz r5, BOr r5 cdz r,
>           BNot r nr)
>     => Nondimensional (Dimension m kg s a k mol cd) nr where
>     nondimensional (Dimension m kg s a k mol cd) =
>         bnot $ nonzero m `bor` nonzero kg `bor` nonzero s `bor` nonzero a
>                  `bor` nonzero k `bor` nonzero mol `bor` nonzero cd

Type value

> instance (TypeValue m, TypeValue kg, TypeValue s, TypeValue a,
>           TypeValue k, TypeValue mol, TypeValue cd)
>     => TypeValue (Dimension m kg s a k mol cd) where
>     typeValue = Dimension (typeValue :: m) (typeValue :: kg) (typeValue :: s)
>         (typeValue :: a) (typeValue :: k) (typeValue :: mol) (typeValue :: cd)
