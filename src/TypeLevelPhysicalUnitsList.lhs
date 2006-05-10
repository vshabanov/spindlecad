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

Some physical units.

Remark: conversion factors are taken
from http://www.electro-optical.com/unitconv/unitdict/units_cat.htm


> module TypeLevelPhysicalUnitsList where

> import TypeLevelInteger
> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue

Squares

> square a = a *. a

> type Meter2 = Dimension (Succ One) Zero Zero Zero Zero Zero Zero
> meter2    = square meter
> kilogram2 = square kilogram
> second2   = square second
> ampere2   = square ampere   
> kelvin2   = square kelvin   
> mole2     = square mole     
> candela2  = square candela  

Cubes

> cube a = a *. a *. a

> type Meter3 = Dimension (Succ (Succ One)) Zero Zero Zero Zero Zero Zero
> meter3    = cube meter
> kilogram3 = cube kilogram
> second3   = cube second
> ampere3   = cube ampere   
> kelvin3   = cube kelvin   
> mole3     = cube mole     
> candela3  = cube candela  

Fourths

> type Meter4 = Dimension (Succ (Succ (Succ One))) Zero Zero Zero Zero Zero Zero
> meter4    = square meter2
> kilogram4 = square kilogram2
> second4   = square second2
> ampere4   = square ampere2  
> kelvin4   = square kelvin2
> mole4     = square mole2
> candela4  = square candela2

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

Unsorted

> mm = milli meter
> kN = kilo newton
