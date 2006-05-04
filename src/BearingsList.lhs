List of bearings and their parameters

> module BearingsList where

> import Bearing
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList

List of all bearings defined below

> bearings = [ fagB7012C_2RSD_T_P4S_UL,
>              fagB7015C_2RSD_T_P4S_UL
>            ]

FAG hi-precision spindle bearings

> fagB7012C_2RSD_T_P4S_UL = AxialBearing -- page 40-41
>     { manufacturer = "FAG",
>       name = "B7012C.2RSD.T.P4S.UL", -- sealed design, ISO P4, light series
>       -- code = "B7012C.T.P4S"
>       contactAngle = 15 * degree,
>       innerDiameter = 60 .* mm,
>       outerDiameter = 95 .* mm,
>       width = 18 .* mm,
>       dynamicLoad = 39 .* kN,
>       staticLoad = 33.5 .* kN,
>       attainableSpeedGrease = 14000 .* rpm,
>       attainableSpeedOil = 22000 .* rpm,
>       preloadingForce = 211 .* newton,
>       unloadingForce = 658 .* newton,
>       axialRigidity = 64.5 .* newton /. micro meter
>     }

> fagB7015C_2RSD_T_P4S_UL = AxialBearing -- page 46-47
>     { manufacturer = "FAG",
>       name = "B7015C.2RSD.T.P4S.UL", -- sealed design, ISO P4, light series
>       -- code = "B7015C.T.P4S"
>       contactAngle = 15 * degree,
>       innerDiameter = 75 .* mm,
>       outerDiameter = 115 .* mm,
>       width = 20 .* mm,
>       dynamicLoad = 51 .* kN,
>       staticLoad = 46.5 .* kN,
>       attainableSpeedGrease = 12000 .* rpm,
>       attainableSpeedOil = 19000 .* rpm,
>       preloadingForce = 283 .* newton,
>       unloadingForce = 880 .* newton,
>       axialRigidity = 76.8 .* newton /. micro meter
>     }
