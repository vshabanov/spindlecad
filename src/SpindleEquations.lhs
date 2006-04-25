This module contains utility functions useful for creating
system of equation which describe particular spindle.

> module SpindleEquations where

> import CASExpr
> import Material


Moment inertia of circle

> jCircle d = (cas_pi `Divide` Integer 64) `Multiply` Expt d (Integer 4)
