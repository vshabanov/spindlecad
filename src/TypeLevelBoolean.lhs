> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level booleans.

> module TypeLevelBoolean where

See TypeLevelInteger.lhs for details about function level => type level
mapping.

    data Boolean = BTrue | BFalse

Boolean data contructors and type class:

> data BTrue  = BTrue  deriving (Eq, Ord, Show)
> data BFalse = BFalse deriving (Eq, Ord, Show)

> class Boolean x
> instance Boolean BTrue
> instance Boolean BFalse

The End.
