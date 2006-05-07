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

> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

This module contains utility functions useful for creating
system of equations that describes particular spindle.

> module SpindleEquations where

> import CASExpr hiding (eval)
> import qualified CASExpr (eval)
> import Material
> import MaterialsList
> import Bearing
> import BearingsList
> import Maxima
> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList
> import Data.Ratio
> import Text.Printf
> import qualified Data.Map as Map
> import qualified ThirdParty.Era as Era


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
>     divEJ desc $ s "A0" + s "A1" * x + s "A2" * x**2 + s "A3" * x**3
>     where s = symbol desc
>           x = s "x"

> divEJ  (prefix, e, j) = (/ (pv e pascal * pv j meter4))
> symbol (prefix, e, j) a = Symbol (prefix ++ a)
> prefix (p, _, _) = p

Partial solutions.

Radial force
    -P/6*(x-a)^3
    ------------
        E*J

> partialSolutionRadialForce desc force coordinate =
>     divEJ desc $ -pv force newton/6 * (x-pv coordinate meter)**3
>     where s = symbol desc
>           x = s "x"

Bending moment
    -M/2*(x-a)^2
    ------------
        E*J

> partialSolutionBendingMoment desc moment coordinate =
>     divEJ desc $ -pv moment (newton *. meter)/2 * (x-pv coordinate meter)**2
>     where s = symbol desc
>           x = s "x"

Boundary conditions.
  
Free end.
    y'' = 0, y''' = 0

> freeEnd desc coordinate rhs =
>     [subst [(x, pv coordinate meter)] (diffn rhs x 2) `Equal` 0,
>      subst [(x, pv coordinate meter)] (diffn rhs x 3) `Equal` 0]
>     where x = prefix desc ++ "x"

Radial bearing
    y = R/j

> radialBearing desc coordinate r j rhs =
>     [(pv r newton / pv j (newton /. meter))
>      `Equal` subst [(x, pv coordinate meter)] rhs]
>     where x = prefix desc ++ "x"


Moment inertia of circle
    pi/64*d^4

> jCircle d = pi/64 * pv d meter**4

Areas.

> areaOfCircle d = pi/4 * pv d meter**2


Utility type class which convert physical values to CAS values
and don't change values if they are already CASExpr.

> pv v t = physicalValueToCASExpr v t

> class PhysicalValueToCASExpr v t where
>     physicalValueToCASExpr :: v -> t -> CASExpr

> instance PhysicalValueToCASExpr CASExpr (Value a) where
>     physicalValueToCASExpr a _ = a
> instance (DimensionDivide a a NonDim) =>
>     PhysicalValueToCASExpr (Value a) (Value a) where
>     physicalValueToCASExpr a d = a /. d


--------------------------------------------------------------------------------

Test case #1.
Spindle with 1N force on the end of console part
and two radial GOST 46120 bearings.

Scheme:
    | 1N
    |    _       _
    V    o       o
    ----------------
         o       o
         -       -

Parameters:
    Shaft diameter              100 mm
    Console length              100 mm
    Bay lengh                   800 mm
    Bearing radial rigidity     120 N/mum

> testCase1 = withInterpreter $ \i -> do
>     let d = 100 .* mm
>         sj = jCircle d
>         e = modulusOfElasticity steel
>         desc = ("", e, sj)
>         c = 0 .* mm
>         b = 100 .* mm
>         a = 800 .* mm
>         j = 120 .* newton /. micro meter
>         --j = 12 .* kgf /. micro meter
>         s = generalSolution desc
>         sf = s  + partialSolutionRadialForce desc (1.*newton) c
>         s1 = sf + partialSolutionRadialForce desc (Symbol "R1") b
>         s2 = s1 + partialSolutionRadialForce desc (Symbol "R2") (a+.b)
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
>     mapM_ (\ i -> do let y = (CASExpr.eval $
>                               substitute (Map.fromList [("X", i)]) y0)::Era.CR
>                      --printf "%.5f\n" $ y * (meter /. micro meter)))
>                      print $ y * CASExpr.eval (meter /. micro meter))
>               (map (* (mm /. meter)) [0,10..100])


Test case #2.
Spindle with 1N force in the middle of bay part
and two axial FAG B7015C.T.P4S bearings.

Scheme:
            | 1N
            |
       o\   V    /o
    ----------------
       o/        \o

Parameters:
    Shaft diameter              75 mm
    Console length              100 mm
    Bay lengh                   800 mm
    Bearings axial rigidity     76.8 N/mum

> testCase2 = withInterpreter $ \i -> do
>     let bearing = fagB7015C_2RSD_T_P4S_UL
>         d = innerDiameter bearing
>         sj = jCircle d
>         s = areaOfCircle d
>         e = modulusOfElasticity steel
>         desc = ("", e, sj)
>         c = 500 .* mm
>         b = 100 .* mm
>         a = 800 .* mm
>         ja = axialRigidity bearing
>         j = 6 .* ja -- radial regidity calculated using FAG recommendations
>         y = generalSolution desc
>         y1  = y   + partialSolutionRadialForce desc (Symbol "R1") b
>         y1f = y1  + partialSolutionRadialForce desc (1.*newton) c
>         y2  = y1f + partialSolutionRadialForce desc (Symbol "R2") (a+.b)
>     -- to check ourselfes we first solve this spindle
>     -- as one with two radial bearings
>     let eqlistAsRadial = (freeEnd desc (0.*mm) y ++
>                           freeEnd desc (a+.b) y2 ++
>                           radialBearing desc b (Symbol "R1") j y1 ++
>                           radialBearing desc (a+.b) (Symbol "R2") j y2)
>     asRadial <- eval i $ solve eqlistAsRadial ["A0", "A1", "A2", "A3",
>                                                "R1", "R2"]
>     printValues i asRadial y   "X" [0,10..100]
>     printValues i asRadial y1  "X" [100,200..500]
>     printValues i asRadial y1f "X" [500,600..900]
>     printValues i asRadial y2  "X" [900,1000]


printValues prints `function` results (in mum) for `parameter`
in `list` of values (in mm). `solution` is used to substitute all
other parameters

> printValues i solution function parameter list = do
>     let List [a] = solution
>     f <- eval i $ Funcall CFSubst [a, function]
>     mapM_ (\ i -> do let y = (CASExpr.eval $ substitute
>                               (Map.fromList [(parameter, i)]) $
>                               f * (meter /. micro meter)) :: Era.CR
>                      putStrLn (show (truncate $ CASExpr.eval $
>                                      i .* meter /. mm)
>                                ++ "\t" ++ show y))
>               (map (* (mm /. meter)) list)
