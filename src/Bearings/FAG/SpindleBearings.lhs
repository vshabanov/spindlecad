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

List of bearings and their parameters

> module Bearings.FAG.SpindleBearings where

> import Bearing
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList

List of all bearings defined below

> bearings = [ fagB7012C_T_P4S_UL,
>              fagB7015C_T_P4S_UL
>            ]

FAG hi-precision spindle bearings

> fagB7012C_T_P4S_UL = Bearing -- page 40-41
>     { manufacturer = "FAG",
>       code = "B7012C.T.P4S",
>       designation = "B7012C.T.P4S.UL",
>       -- standard bearing w/steel balls (B), medium series (70),
>       -- sigle bearing (U), light preload (L)
>       --name = "B7012C.2RSD.T.P4S.UL", -- sealed design (2RSD grease used)
>       bearingType = "Standard. Steel balls.",
>       dimensionSeries = MediumDimensionSeries,
>       contactAngle = 15 * degree,
>       innerDiameter = 60 .* mm,
>       outerDiameter = 95 .* mm,
>       width = 18 .* mm,
>       attainableSpeedGrease = 14000 .* rpm,
>       attainableSpeedOil = 22000 .* rpm,
>       radialRigidity = (64.5/2*6) .* newton /. micro meter,
>       axialRigidity = (64.5/2) .* newton /. micro meter
>                         -- ^ half because of Sa given for bearing pair
>     }

> fagB7015C_T_P4S_UL = Bearing -- page 46-47
>     { manufacturer = "FAG",
>       code = "B7015C.T.P4S",
>       designation = "B7015C.T.P4S.UL",
>       bearingType = "Standard. Steel balls.",
>       dimensionSeries = MediumDimensionSeries,
>       contactAngle = 15 * degree,
>       innerDiameter = 75 .* mm,
>       outerDiameter = 115 .* mm,
>       width = 20 .* mm,
>       attainableSpeedGrease = 12000 .* rpm,
>       attainableSpeedOil = 19000 .* rpm,
>       radialRigidity = (76.8/2*6) .* newton /. micro meter,
>       axialRigidity = (76.8/2) .* newton /. micro meter
>     }
