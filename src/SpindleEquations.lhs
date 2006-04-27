> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

This module contains utility functions useful for creating
system of equation which describe particular spindle.

> module SpindleEquations where

> import CASExpr
> import Material
> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList
> import Data.Ratio


The beam can be described using following differential equation

    diff(y, x, 4)*E*J = p(x)

  where   
    y    - deflection at point x
    E    - modulus of elasticty of material
    J    - moment of inertia
    p(x) - distributed load

Solution of this equation is

    y*E*J = A0 + A1*x + A2*x^2 + A3*x^3 + F(x)

  where
    Ai   - constants which depends on solution of equation system
    F(x) - partial solution which depends on force

Equation system consists of boundary conditions for the beam.


General solution
    A0 + A1*x + A2*x^2 + A3*x^3

> generalSolution prefix = s "A0" `Plus`
>                          (s "A1" `Multiply` x) `Plus`
>                          (s "A2" `Multiply` Expt x (Integer 2)) `Plus`
>                          (s "A3" `Multiply` Expt x (Integer 3))
>     where s a = Symbol (prefix ++ a)
>           x = s "x"

Partial solutions.

Radial force
    -P/6*(x-a)^3

> partialSolutionRadialForce prefix force coordinate =
>     (pv force newton `Divide` Integer (-6)) `Multiply`
>     (Expt (x `Minus` pv coordinate meter) (Integer 3))
>     where s a = Symbol (prefix ++ a)
>           x = s "x"

Bending moment
    -M/2*(x-a)^2

> partialSolutionBendingMoment prefix moment coordinate =
>     (pv moment (newton *. meter) `Divide` Integer (-2)) `Multiply`
>     (Expt (x `Minus` pv coordinate meter) (Integer 2))
>     where s a = Symbol (prefix ++ a)
>           x = s "x"

Boundary conditions.

 > freeEnd


Moment inertia of circle

> jCircle d = (cas_pi `Divide` Integer 64) `Multiply` Expt (pv d meter) (Integer 4)


Utility type class which convert physical values to CAS values
and don't change values if they are already CASExpr.

> pv v t = physicalValueToCASExpr v t

> class PhysicalValueToCASExpr v t where
>     physicalValueToCASExpr :: v -> t -> CASExpr

> instance PhysicalValueToCASExpr CASExpr (Value a) where
>     physicalValueToCASExpr a _ = a
> instance (DimensionDivide a a NonDim) =>
>     PhysicalValueToCASExpr (Value a) (Value a) where
>     physicalValueToCASExpr a d =
>         -- we convert values to integral representation since
>         -- solution of system with integer is faster
>         -- and doesn't suffer from round-off errors
>         if denominator v == 1
>            then Integer (numerator v)
>            else Rational v
>         where v = approxRational (a /. d) 1e-17
>                   -- approxRational is necessary in converting values
>                   -- like 0.1 that doesn'y have finite floating point
>                   -- representation.
>                   -- It won't work with too small numbers, but
>                   -- its ok for our numbers (we don't calculate angstrems :)

