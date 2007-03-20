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

> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Some physical units.

Remark: conversion factors are taken
from http://www.electro-optical.com/unitconv/unitdict/units_cat.htm


> module TekHaskell.TypeLevelPhysicalUnitsList where

> import TekHaskell.TypeLevelInteger
> import TekHaskell.TypeLevelPhysicalDimension
> import TekHaskell.TypeLevelPhysicalValue

Squares

> square :: (DimensionMultiply d d d2) => Value d -> Value d2
> square (Value a) = Value (a**2)

> type Meter2 = Dimension (Succ One) Zero Zero Zero Zero Zero Zero
> meter2    = square meter
> kilogram2 = square kilogram
> second2   = square second
> ampere2   = square ampere   
> kelvin2   = square kelvin   
> mole2     = square mole     
> candela2  = square candela  

Cubes

> cube :: (DimensionMultiply d d d2, DimensionMultiply d d2 d3)
>         => Value d -> Value d3
> cube (Value a) = Value (a**3)

> type Meter3 = Dimension (Succ (Succ One)) Zero Zero Zero Zero Zero Zero
> meter3    = cube meter
> kilogram3 = cube kilogram
> second3   = cube second
> ampere3   = cube ampere   
> kelvin3   = cube kelvin   
> mole3     = cube mole     
> candela3  = cube candela  

Fourths

> fourth :: (DimensionMultiply d d d2, DimensionMultiply d2 d2 d4)
>           => Value d -> Value d4
> fourth (Value a) = Value (a**4)

> type Meter4 = Dimension (Succ (Succ (Succ One))) Zero Zero Zero Zero Zero Zero
> meter4    = fourth meter
> kilogram4 = fourth kilogram
> second4   = fourth second
> ampere4   = fourth ampere  
> kelvin4   = fourth kelvin
> mole4     = fourth mole
> candela4  = fourth candela

Time.

> minute = 60 .* second
> hour   = 60 .* minute
> day    = 24 .* hour

Force.

> type Newton = Dimension One One (Neg (Succ One)) Zero Zero Zero Zero
> newton, kgf :: Value Newton
> newton = kilogram *. meter /. second2
> kgf = 9.80665 .* newton

Pressure.

> type Pascal = Dimension (Neg One) One (Neg (Succ One)) Zero Zero Zero Zero
> pascal :: Value Pascal
> pascal = newton /. meter2

Volume.

> liter = 0.001 .* meter3
> barrel = 158.987294928 .* liter -- just for fun

Rigidity

> type NewtonDivMeter = Dimension Zero One (Neg (Succ One)) Zero Zero Zero Zero

Bending Moment

> type NewtonMulMeter =
>     Dimension (Succ One) One (Neg (Succ One)) Zero Zero Zero Zero

Rotational speed, frequency

> type Hertz = Dimension Zero Zero (Neg One) Zero Zero Zero Zero
> hertz, rpm :: Value Hertz
> hertz = 1 ./ second
> rpm = 1 ./ minute

Angle

> radian, degree :: NondimensionalValue
> radian = 1
> degree = pi / 180

Energy

> type Joule = 
>     Dimension (Succ One) One (Neg (Succ One)) Zero Zero Zero Zero
> joule :: Value Joule
> joule = newton *. meter

Power

> type Watt =
>     Dimension (Succ One) One (Neg (Succ (Succ One))) Zero Zero Zero Zero
> watt :: Value Watt
> watt = joule /. second

Electricity

> type Volt =
>     Dimension (Succ One) One (Neg (Succ (Succ One))) (Neg One) Zero Zero Zero
> volt :: Value Volt
> volt = watt /. ampere

Unsorted

> mm = milli meter
> cm = centi meter
> kN = kilo newton
