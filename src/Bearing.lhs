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

Bearing description data type

> module Bearing where

> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList

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


> data Bearing = AngularContactBearing
>                { manufacturer :: String,
>                  name :: String,
>                  contactAngle :: NondimensionalValue,
>                  innerDiameter :: Value Meter, -- d
>                  outerDiameter :: Value Meter, -- D
>                  width :: Value Meter, -- B
>                  dynamicLoad :: Value Newton, -- Cdyn
>                  staticLoad :: Value Newton,  -- C0stat
>                  attainableSpeedGrease :: Value Hertz,
>                  attainableSpeedOil :: Value Hertz,
>                  preloadingForce :: Value Newton, -- Fv
>                  unloadingForce :: Value Newton,  -- KaE
>                  axialRigidity :: Value NewtonDivMeter -- Sa
>                }
>              | RadialBearing
>                { manufacturer :: String,
>                  name :: String,
>                  innerDiameter :: Value Meter, -- d
>                  outerDiameter :: Value Meter, -- D
>                  width :: Value Meter, -- B
>                  dynamicLoad :: Value Newton, -- Cdyn
>                  staticLoad :: Value Newton,  -- C0stat
>                  attainableSpeedGrease :: Value Hertz,
>                  attainableSpeedOil :: Value Hertz,
>                  radialRigidity :: Value NewtonDivMeter
>                }
>     deriving (Eq, Ord, Show)


Bad but at the moment useful util, which converts angular bearing
to radial one using specified conversion coefficient from axial rigidity.

> radialBearingFromAngular c b =
>     RadialBearing { manufacturer = manufacturer b,
>                     name = name b,
>                     innerDiameter = innerDiameter b,
>                     outerDiameter = outerDiameter b,
>                     width = width b,
>                     dynamicLoad = dynamicLoad b,
>                     staticLoad = staticLoad b,
>                     attainableSpeedGrease = attainableSpeedGrease b,
>                     attainableSpeedOil = attainableSpeedOil b,
>                     radialRigidity = c .* axialRigidity b 
>                   }

Radial bearing with only radialRigidity defined

> radialBearingJ j =
>     RadialBearing { manufacturer = undefined,
>                     name = undefined,
>                     innerDiameter = undefined,
>                     outerDiameter = undefined,
>                     width = undefined,
>                     dynamicLoad = undefined,
>                     staticLoad = undefined,
>                     attainableSpeedGrease = undefined,
>                     attainableSpeedOil = undefined,
>                     radialRigidity = j
>                   }
