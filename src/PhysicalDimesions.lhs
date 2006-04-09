> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level representation of physical dimenstions.

> module PhysicalDimensions where

The dimension is represented as tuple containing powers of all basic SI units.
Powers are type level numbers from

> import TypeLevelInteger
> import TypeLevelRatio

Definitions of the Seven Basic SI Units

meter [m]
    The meter is the basic unit of length. It is the distance light travels,
    in a vacuum, in 1/299,792,458th of a second. 
kilogram [kg]
    The kilogram is the basic unit of mass. It is the mass of an international
    prototype in the form of a platinum-iridium cylinder kept at Sevres
    in France. It is now the only basic unit still defined in terms of a
    material object, and also the only one with a prefix[kilo] already in place.
second [s]
    The second is the basic unit of time. It is the length of time taken
    for 9,192,631,770 periods of vibration of the caesium-133 atom to occur. 
ampere [A]
    The ampere is the basic unit of electric current. It is that current which
    produces a specified force between two parallel wires which are 1 meter
    apart in a vacuum.
    It is named after the French physicist Andre Ampere (1775-1836). 
kelvin [K]
    The kelvin is the basic unit of temperature. It is 1/273.16th of the
    thermodynamic temperature of the triple point of water.
    It is named after the Scottish mathematician and physicist
    William Thomson 1st Lord Kelvin (1824-1907).
mole [mol]
    The mole is the basic unit of substance. It is the amount of substance
    that contains as many elementary units as there are
    atoms in 0.012 kg of carbon-12. 
candela [cd]
    The candela is the basic unit of luminous intensity. It is the intensity
    of a source of light of a specified frequency, which gives a specified
    amount of power in a given direction. 


> data Dimension m kg s a k mol cd = Dimension m kg s a k mol cd
>                                    deriving (Eq,Show,Read)

