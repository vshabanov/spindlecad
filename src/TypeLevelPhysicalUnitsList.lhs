> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Some physical units

> module TypeLevelPhysicalUnitsList where

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

Unsorted

> rpm = 1 ./ minute
> newton = kilogram *. meter /. second2
> pascal = newton /. meter2
