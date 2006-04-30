> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

This module contains utility functions useful for creating
system of equations that describes particular spindle.

> module SpindleEquations where

> import CASExpr
> import Material
> import MaterialsList
> import Maxima
> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList
> import Data.Ratio
> import Text.Printf
> import qualified Data.Map as Map


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
    ---------------------------
               E*J

> generalSolution desc =
>     divEJ desc $
>      s "A0" `Plus`
>      (s "A1" `Multiply` x) `Plus`
>      (s "A2" `Multiply` Expt x (Integer 2)) `Plus`
>      (s "A3" `Multiply` Expt x (Integer 3))
>     where s = symbol desc
>           x = s "x"

> divEJ  (prefix, e, j) = (`Divide` (pv e pascal `Multiply` pv j meter4))
> symbol (prefix, e, j) a = Symbol (prefix ++ a)
> prefix (p, _, _) = p

Partial solutions.

Radial force
    -P/6*(x-a)^3
    ------------
        E*J

> partialSolutionRadialForce desc force coordinate =
>     divEJ desc $
>     (pv force newton `Divide` Integer (-6)) `Multiply`
>     (Expt (x `Minus` pv coordinate meter) (Integer 3))
>     where s = symbol desc
>           x = s "x"

Bending moment
    -M/2*(x-a)^2
    ------------
        E*J

> partialSolutionBendingMoment desc moment coordinate =
>     divEJ desc $
>     (pv moment (newton *. meter) `Divide` Integer (-2)) `Multiply`
>     (Expt (x `Minus` pv coordinate meter) (Integer 2))
>     where s = symbol desc
>           x = s "x"

Boundary conditions.
  
Free end.
    y'' = 0, y''' = 0

> freeEnd desc coordinate rhs =
>     [subst [(x, pv coordinate meter)] (diffn rhs x 2) `Equal` Integer 0,
>      subst [(x, pv coordinate meter)] (diffn rhs x 3) `Equal` Integer 0]
>     where x = prefix desc ++ "x"

Radial bearing
    y = R/j

> radialBearing desc coordinate r j rhs =
>     [(pv r newton `Divide` pv j (newton /. meter))
>      `Equal` subst [(x, pv coordinate meter)] rhs]
>     where x = prefix desc ++ "x"

Test case

> testCase = withInterpreter $ \i -> do
>     let mm = milli meter
>         d = 100 .* mm
>         sj = jCircle d
>         e = modulusOfElasticity steel
>         desc = ("", e, sj)
>         c = 0 .* mm
>         b = 100 .* mm
>         a = 800 .* mm
>         j = 120 .* newton /. micro meter
>         --j = 12 .* kgf /. micro meter
>         s = generalSolution desc
>         sf = s  `Plus` partialSolutionRadialForce desc (1.*newton) c
>         s1 = sf `Plus` partialSolutionRadialForce desc (Symbol "R1") b
>         s2 = s1 `Plus` partialSolutionRadialForce desc (Symbol "R2") (a+.b)
>     let eqlist = --map (substitute constants)--Map.empty)
>                  (freeEnd desc c s ++
>                   freeEnd desc (a+.b) s2 ++
>                   radialBearing desc b (Symbol "R1") j s1 ++
>                   radialBearing desc (a+.b) (Symbol "R2") j s2)
>     --print eqlist
>     r <- eval i $ solve eqlist ["A0", "A1", "A2", "A3", "R1", "R2"]
>     --print r
>     let List [a] = r
>     y0 <- eval i $ Funcall CFSubst [a, sf]
>     --print y0
>     mapM_ (\ i -> do let Double y = substitute (Map.insert "X" (Double i) constants) y0
>                      printf "%.5f\n" $ y .* meter /. micro meter)
>               (map (* (mm /. meter)) [0,10..100])

Moment inertia of circle
    pi/64*d^4

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

