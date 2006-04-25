Material data type that contain some common physical properties

> module Material where

> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList

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