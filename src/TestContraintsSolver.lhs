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

The test contraints solver.

This module is used for experiments with primitive contraints systems creation
and solving.

> module TestConstraintsSolver where

> import CASExpr

space1 = do let a = symbol "A"
                b = symbol "C"
            a .==. b + 2
            a .==. 2
 ==> b == 0, a == 2

space2 = do let x = symbol "x"
                y = symbol "y"
            x ^ 2 .==. y

 ==> x == [sqrt y, -(sqrt y)], y == x^2

space3 = do space space1 .==. symbol "s1"
            space space2 .==. symbol "s2"
            symbol "s1" .->. symbol "b" .==. var "s2.x"

 ==> s2.y == 0

