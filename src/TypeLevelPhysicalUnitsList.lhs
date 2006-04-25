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

> meter2    = square meter
> kilogram2 = square kilogram
> second2   = square second
> ampere2   = square ampere   
> kelvin2   = square kelvin   
> mole2     = square mole     
> candela2  = square candela  

Cubes

> cube a = a *. a *. a

> meter3    = cube meter
> kilogram3 = cube kilogram
> second3   = cube second
> ampere3   = cube ampere   
> kelvin3   = cube kelvin   
> mole3     = cube mole     
> candela3  = cube candela  

Time.

> minute = 60 .* second
> hour   = 60 .* minute
> day    = 24 .* hour

Force.

> newton = kilogram *. meter /. second2
> kgf = 9.80665 .* newton

Pressure.

> type Pascal = Dimension (Neg One) One (Neg (Succ One)) Zero Zero Zero Zero
> pascal :: Value Pascal
> pascal = newton /. meter2

Volume.

> liter = 0.001 .* meter3
> barrel = 158.987294928 .* liter -- just for fun

Unsorted

> rpm = 1 ./ minute
