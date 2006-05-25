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

Bearing description data type

> module Bearing where

> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList
> import CASExpr

Bearing data type.

TODO: there will be run-time error if we call axialRigidity on RadialBearing
And due to Haskell lazyness this bug can appear at any time at any place.
We need separate types for angular and radial bearings or some other way
to eliminate above error (make it compile time).
The bearing type is compound
  - angular/radial/axial
  - taper/cylindrical
  - precision class
  - oil/grease/oilAndGrease
  - preload necessary
  - etc.
Any of this components may be absent, but algoritms which don't need them
must still work.

For the moment we have simple all-in-one type.

> data Bearing = Bearing
>                { manufacturer :: String,
>                  code :: String,
>                  designation :: String,
>                  bearingType :: String,
>                  dimensionSeries :: DimensionSeries,
>                  contactAngle :: NondimensionalValue,
>                  innerDiameter :: Value Meter, -- d
>                  outerDiameter :: Value Meter, -- D
>                  width :: Value Meter, -- B
>                  attainableSpeedGrease :: Value Hertz,
>                  attainableSpeedOil :: Value Hertz,
>                  radialRigidity :: Value NewtonDivMeter,
>                  axialRigidity :: Value NewtonDivMeter, -- Sa
>                  cdyn :: Value Newton,  -- Cdyn   - dynamic load rating
>                  c0stat :: Value Newton, -- C0stat - static load rating
>                  innerRingRadialRunout :: Value Meter,
>                  outerRingRadialRunout :: Value Meter
>                }
>     deriving (Eq, Ord, Show)

Bearing dimension series.

> data DimensionSeries = UltraLightweightDimensionSeries
>                      | LightweightDimensionSeries
>                      | MediumDimensionSeries
>                      | HeavyDimensionSeries
>     deriving (Eq, Ord, Show)


> radialBearingJ j =
>     Bearing { manufacturer = undefined,
>               code = "Radial bearing",
>               designation = "Radial bearing, j = "
>                             ++ show (eval $ j /. (newton/.micro meter))
>                             ++ "N/mum",
>               bearingType = "Radial bearing",
>               dimensionSeries = undefined,
>               contactAngle = undefined,
>               innerDiameter = undefined,
>               outerDiameter = undefined,
>               width = undefined,
>               attainableSpeedGrease = undefined,
>               attainableSpeedOil = undefined,
>               radialRigidity = j,
>               axialRigidity = undefined,
>               cdyn = undefined,
>               c0stat = undefined,
>               innerRingRadialRunout = undefined,
>               outerRingRadialRunout = undefined
>             }
