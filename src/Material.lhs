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

Material data type that contain some common physical properties

> module Material where

> import TekHaskell

Material data type.
TODO: maybe rewrite this in type classes?
Not necessary now (especially in case when we need some search
for appropriate material). But type classes and materials as types
can be useful for compile time checks. We can combine two approaches -
make generation of material sum type from material with specific
type classes. But again - not now!

> data Material =
>     IsotropicMaterial
>     { modulusOfElasticity :: Value Pascal -- E
>     }
>     deriving (Eq, Ord, Show)