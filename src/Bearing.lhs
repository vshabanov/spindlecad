Bearing description data type

> module Bearing where

> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList

Bearing data type.

> data Bearing =
>     AxialBearing
>     { manufacturer :: String,
>       name :: String,
>       contactAngle :: NondimensionalValue,
>       innerDiameter :: Value Meter, -- d
>       outerDiameter :: Value Meter, -- D
>       width :: Value Meter, -- B
>       dynamicLoad :: Value Newton, -- Cdyn
>       staticLoad :: Value Newton,  -- C0stat
>       attainableSpeedGrease :: Value Hertz,
>       attainableSpeedOil :: Value Hertz,
>       preloadingForce :: Value Newton, -- Fv
>       unloadingForce :: Value Newton,  -- KaE
>       axialRigidity :: Value NewtonDivMeter -- Sa
>     }
>     deriving (Eq, Ord, Show)