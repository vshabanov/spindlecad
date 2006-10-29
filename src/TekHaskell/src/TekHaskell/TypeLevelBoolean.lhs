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

Type level booleans.

> module TekHaskell.TypeLevelBoolean where

See TypeLevelInteger.lhs for details about function level => type level
mapping.

    data Boolean = BTrue | BFalse

Boolean data contructors and type class:

> data BTrue  = BTrue  deriving (Eq, Ord, Show)
> data BFalse = BFalse deriving (Eq, Ord, Show)

> class Boolean x
> instance Boolean BTrue
> instance Boolean BFalse

Value getting for type

> class TypeValue x where
>     typeValue :: x

> instance TypeValue BTrue where
>     typeValue = BTrue
> instance TypeValue BFalse where
>     typeValue = BFalse

Boolean operations.

    not

> class (Boolean a, Boolean b) => BNot a b | a -> b where
>     bnot :: a -> b

> instance BNot BFalse BTrue where
>     bnot BFalse = BTrue
> instance BNot BTrue BFalse where
>     bnot BTrue = BFalse

    or

> class (Boolean a, Boolean b, Boolean c) => BOr a b c | a b -> c where
>     bor :: a -> b -> c

> instance BOr BFalse BFalse BFalse where
>     bor BFalse BFalse = BFalse
> instance BOr BFalse BTrue BTrue where
>     bor BFalse BTrue = BTrue
> instance BOr BTrue BFalse BTrue where
>     bor BTrue BFalse = BTrue
> instance BOr BTrue BTrue BTrue where
>     bor BTrue BTrue = BTrue

The End.
