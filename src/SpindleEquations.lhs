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
>                               substitute (Map.fromList [("x", i)]) y0)
>                              ::Era.CR
>                      --printf "%.5f\n" $ y * (meter /. micro meter)))
>                      print $ y * CASExpr.eval (meter /. micro meter))
>               (map (* (mm /. meter)) [0,10..100])


Test case #2. FAILED. Incorrect equations used. Don't see below.
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
>         ca = contactAngle bearing
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
>     printSolvedExpr i     "y_0  = " asRadial [("x",0)] y
>     y' <- eval i $ diff y "x"
>     printSolvedExpr i     "y'_0 = " asRadial [("x",0)] y'
>     printSolvedExpr i     "y_R1 = " asRadial [("x",0.1)] y1
>     printSolvedExpr i     "y_R2 = " asRadial [("x",0.9)] y2
>     printSolvedVariable i "R1   = " asRadial "R1"
>     printSolvedVariable i "R2   = " asRadial "R2"
>     --printValues i asRadial y   "x" [0,10..100]
>     --printValues i asRadial y1  "x" [100,200..500]
>     --printValues i asRadial y1f "x" [500,600..900]
>     --printValues i asRadial y2  "x" [900,1000]
>     let x = Symbol "x"
>         r = Symbol "R" -- axial reaction
>         zr  = Symbol "C" + x
>         zr1 = Symbol "C" + x - (x - pv b meter)*r/(pv e pascal*(pv s meter2))
>         zr2 = Symbol "C" + x - (pv a meter)*r/(pv e pascal*(pv s meter2))
>         n = 10            -- n balls in bearing (one side only)
>         ji = (1/n) .* ja  -- rigidity of one ball
>         angles = map (pi/(n-1)*) [0..n-1]
>         r1i = map (ballReactions ca d b zr y ji) angles
>         r2i = map (ballReactions (-ca) d (b+.a) zr1 y1f ji) angles
>         ra1 = foldl (+) 0 $ map fst r1i
>         ra2 = foldl (+) 0 $ map fst r2i
>         rr1 = foldl (+) 0 $ map snd r1i
>         rr2 = foldl (+) 0 $ map snd r2i
>     let eqlistAngular = (freeEnd desc (0.*mm) y ++
>                          freeEnd desc (a+.b) y2 ++
>                          [ra1 `Equal` r] ++
>                          [ra2 `Equal` (-r)] ++
>                          [rr1 `Equal` Symbol "R1"] ++
>                          [rr2 `Equal` Symbol "R2"] ++
>                          [subst [("x", pv c meter)] zr1 `Equal` pv c meter])
>     --asAngular <- eval i $ solve eqlistAngular ["A0", "A1", "A2", "A3",
>     --                                            "R", "C", "R1", "R2"]
>     --print asAngular -- ^^ this system is not solved
>     ---------------
>     let br = map (ballReactions ca d b
>                   (Symbol "Z0" + x) (Symbol "A0" + Symbol "A1" * x) ji) angles
>         bra = foldl (+) 0 $ map fst br
>         brr = foldl (+) 0 $ map snd br
>         eqlist = [bra `Equal` 1, -- 1N axial force
>                   brr `Equal` 0] -- no radial force
>     --solution <- eval i $ solve eqlist ["A0", "A1", "Z0"]
>     --print solution -- it solves only for two bearings (n=2)
>                    -- maybe doesn't sum reactions, but only integrate?
>     -- we use y0 and y'0 from previous calculations, z0 was searched manually
>     -- so brr~=0.5 and this z is much (46 times) greater than it should be
>     -- after axial deformation by bra.
>     printSolvedExpr i "bra = " (List [List []])
>                         [("Z0",0.000000046), ("A0", -1.085e-9), ("A1", -1.22e-7)] bra
>     printSolvedExpr i "brr = " (List [List []])
>                         [("Z0",0.000000046), ("A0", -1.085e-9), ("A1", -1.22e-7)] brr
>     print $ CASExpr.eval $ (pv a meter)*1.17/(pv e pascal*(pv s meter2))

> ballReactions alpha d b zr y ji beta = (rai, rri)
>     where xi = pv d meter / 2 * cos beta
>           yi = pv d meter / 2 * sin beta
>           substb = subst [("x", pv b meter)]
>           phi = substb (diff y "x")
>           yiabs = yi * cos phi + substb y
>           ziabs = yi * sin phi + substb zr
>           r = sqrt $ xi**2 + yiabs**2
>           z0 = pv b meter + pv d meter/(2*tan alpha)
>           zc = z0 - r/tan alpha
>           rai = (ziabs - zc) * pv ji (newton /. meter)
>           rri = (rai / tan alpha) * sin beta


> substSolution i solution pvlist expr = do
>     let List [a] = solution
>     f <- eval i $ Funcall CFSubst [substitute (Map.fromList pvlist) a,
>                                    substitute (Map.fromList pvlist) expr]
>     return $ f

> printSolvedExpr i prefix solution pvlist expr = do
>     v <- substSolution i solution pvlist expr
>     putStrLn (prefix ++ show (CASExpr.eval v {-:: Era.CR-}))

> printSolvedVariable i prefix solution symbol = do
>     v <- substSolution i solution [] (Symbol symbol)
>     putStrLn (prefix ++ show (CASExpr.eval v :: Era.CR))

printValues prints `function` results (in mum) for `parameter`
in `list` of values (in mm). `solution` is used to substitute all
other parameters

> printValues i solution function parameter list = do
>     f <- substSolution i solution [] function
>     mapM_ (\ i -> do let y = (CASExpr.eval $ substitute
>                               (Map.fromList [(parameter, i)]) $
>                               f * (meter /. micro meter)) :: Era.CR
>                      putStrLn (show (truncate $ CASExpr.eval $
>                                      i .* meter /. mm)
>                                ++ "\t" ++ show y))
>               (map (* (mm /. meter)) list)
