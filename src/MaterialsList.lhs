List of materials and their properties

> module MaterialsList where

> import Material
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList

Steels.
Default steel

> steel = IsotropicMaterial
>     { modulusOfElasticity = 210000 .* mega pascal
>     }