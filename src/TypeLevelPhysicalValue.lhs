> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level representation of physical values.

> module TypeLevelPhysicalValue where

> import TypeLevelPhysicalDimensions

The phantom type for physical values. It only contain numerical value
but it is type checked at compile time.

> data (Num a) => Value a d = Value a deriving (Eq, Ord, Show)

> kilogram = Value 1 :: Value Integer Kilogram
