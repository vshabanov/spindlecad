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
> import Bearings.FAG.SpindleBearings
> import Maxima
> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList
> import Data.Ratio
> import Text.Printf
> import qualified Data.Map as Map
> import ExactNumber


Spindle description data type.

> type Spindle = [Section]
> data Section = Section { momentOfInertia :: Value Meter4,
>                          material :: Material,
>                          sectionLength :: Value Meter,
>                          forces :: Map.Map (Value Meter) Force,
>                          bearings :: Map.Map (Value Meter) Bearing 
>                        }
>                deriving (Eq, Ord, Show)
> data Force = Force { radialForce :: Value Newton,
>                      bendingMoment :: Value NewtonMulMeter 
>                    }
>              deriving (Eq, Ord, Show)


Spindle construction functions.
All dimensions in millimeters, forces in newtons, moments in newton*meter.
Steel is default material.

Cylindrical section description.

> cylinder :: CASExpr -> CASExpr -> Spindle
> cylinder d l = [Section { momentOfInertia = jCircle (d.*mm) .* meter4,
>                           material = steel,
>                           sectionLength = l.*mm,
>                           forces = Map.empty,
>                           bearings = Map.empty 
>                         }]

Conical section description.

TODO: see comment on equation. to correctly solve conical sections we need to
adjust our equation system generation.
Therefore `cone` is now commented.

 > cone :: CASExpr -> CASExpr -> CASExpr -> Spindle
 > cone d1 d2 l = [Section { momentOfInertia =
 >                           jCircle (((d2-d1)*Symbol "x"/l + d1).*mm) .* meter4,
 >                           material = steel,
 >                           sectionLength = l.*mm,
 >                           forces = Map.empty,
 >                           bearings = Map.empty 
 >                         }]

Something `at` specified coordinate description.

> at :: a -> CASExpr -> (a, Value Meter)
> at a b = (a, b.*mm)

Radial force description.

> addRadialForce :: Spindle -> (CASExpr, Value Meter) -> Spindle
> addRadialForce sp (force, c) = modifySectionAt addrf sp c
>     where addrf s c = s { forces = Map.insert c
>                           (Force { radialForce = force .* newton,
>                                    bendingMoment = 0 .* newton *. meter })
>                           (forces s) }

Bearing description.

> addBearing :: Spindle -> (Bearing, Value Meter) -> Spindle
> addBearing sp (bearing, c) = modifySectionAt addb sp c
>     where addb s c =
>               s { bearings = Map.insert c bearing (bearings s) }

Sequential spindle connection.

> (<+>) :: Spindle -> Spindle -> Spindle
> (<+>) = (++)

Spindle bore description. The bore is just another spindle of equal length
which is cut from base spindle.

> cut :: Spindle -> Spindle -> Spindle
> cut = unionSpindles cutSection
>     where cutSection base bore =
>               base { momentOfInertia =
>                      momentOfInertia base -. momentOfInertia bore }

Fixities for construction functions and operators

> infixl 9 `at`                 -- maximal fixity
> infixl 8 `addRadialForce`     -- same fixity as ^, but left associative
> infixl 8 `addBearing`         -- same fixity as ^, but left associative
> infixl 6 `cut`                -- same fixity as -
> infixr 5 <+>                  -- same fixity as ++

Spindle construction functions utilities.

modifySectionAt find a section at specified coordinate and modify it
using function passed at first argument.

> modifySectionAt :: (Section -> Value Meter -> Section)
>                 -> Spindle -> Value Meter -> Spindle
> modifySectionAt mf [] c = error "modifySectionAt: coordinate too large"
> modifySectionAt mf (x:xs) c =
>     if c <= (sectionLength x)
>        then mf x c : xs
>        else x : modifySectionAt mf xs (c -. sectionLength x)

unionSpindles - unions two spindles using section union function.
    |---1---|---2---|---3---|
             `union`
    |-----1'----|-----2'----|
                =
    |---a---|-b-|-c-|---d---|
where a = sectionUnion 1 (fst (splitSection 1' (length 1)))
      b = sectionUnion (fst (splitSection 2 (length 1' - length 1)))
                       (snd (splitSection 1' (length 1)))
      ..., etc.

> unionSpindles :: (Section -> Section -> Section)
>               -> Spindle -> Spindle -> Spindle
> unionSpindles sectionUnion sp1 sp2 = u sp1 sp2
>     where u [] [] = []
>           u [] a  = error "unionSpindles: spinle1 is shorted than spindle2"
>           u a  [] = error "unionSpindles: spinle2 is shorted than spindle1"
>           u (s1:xs1) (s2:xs2) =
>               if sectionLength s1 == sectionLength s2 then
>                  sectionUnion s1 s2 : u xs1 xs2
>               else if sectionLength s1 < sectionLength s2 then
>                  let (s2a,s2b) = splitSection s2 (sectionLength s1) in
>                  sectionUnion s1 s2a : u xs1 (s2b:xs2)
>               else
>                  let (s1a,s1b) = splitSection s1 (sectionLength s2) in
>                  sectionUnion s1a s2 : u (s1b:xs1) xs2


splitSection splits section at specified coordinate.

> splitSection :: Section -> Value Meter -> (Section, Section)
> splitSection s l =
>     if sectionLength s < l
>        then error "splitSection: section is shorter than specified coordinate"
>        else (s { sectionLength = l,
>                  forces = Map.filterWithKey   (\ k _ -> k <= l) $ forces s,
>                  bearings = Map.filterWithKey (\ k _ -> k <= l) $ bearings s
>                },
>              s { momentOfInertia = substitutepv [("x", Symbol "x" + l/.meter)]
>                    (momentOfInertia s),
>                  sectionLength = sectionLength s -. l,
>                  forces =   Map.mapKeys (\ k -> k -. l) $
>                    Map.filterWithKey (\ k _ -> k > l) $ forces s,
>                  bearings = Map.mapKeys (\ k -> k -. l) $
>                    Map.filterWithKey (\ k _ -> k > l) $ bearings s
>                })


Querying properties of described spindle.

... deflectionLine sp -> [(x,y)]
... reactions sp -> [(x,bearing,r)]
... deflectionOf spindleDecr `at` 0


To get the properties of spindle we first need to solve equation system
corresponding to concrete spindle description.

The spindle is modeled as beam on elastic bearings.

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


TODO: when J (or E) is not constant the solution is changed to one shown below.
We must adjust our equation system generation so it automatically integrate

(%i1) (integrate(integrate(integrate(integrate(y*E(x)*J(x)=p(x),x),x),x),x));
      / / / /                           / / / /
      [ [ [ [                           [ [ [ [
(%o1) I I I I E(x)*J(x)*y dx dx dx dx = I I I I p(x) dx dx dx dx + 
      ] ] ] ]                           ] ] ] ]
      / / / /                           / / / /

      + A0 + A1*x + A2*x^2 + A3*x^3


Equation system consists of boundary conditions for the beam.


General solution
        A0 + A1*x + A2*x^2 + A3*x^3
    y = ---------------------------
                   E*J

> generalSolution desc =
>     divEJ desc $ s "A0" + s "A1" * x + s "A2" * x**2 + s "A3" * x**3
>     where s = symbol desc
>           x = s "x"

> divEJ  (prefix, e, j) = (/ (pv e pascal * pv j meter4))
> symbol (prefix, e, j) a = Symbol (prefix ++ a)
> prefix (p, _, _) = p


Partial solutions.
They are added to deflection (y) at the point where load is changed.
E.g. y_new = y + partialSolution

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

Connected sections.
    y1(l)  = y2(0)                      - deflection equality
    y1'(l) = y2'(0)                     - angles equality
    y1''(l)*E1*J1 = y2''(0)*E2*J2       - moments equality
    y1'''(l)*E1*J1 = y2'''(0)*E2*J2     - forces equality

> connected (prefix1,e1,j1) y1 l (prefix2,e2,j2) y2 =
>     [y1 `eq` y2,
>      diff y1 x1 `eq` diff y2 x2,
>      diffn (y1*e1*j1) x1 2 `eq` diffn (y2*e2*j2) x2 2,
>      diffn (y1*e1*j1) x1 3 `eq` diffn (y2*e2*j2) x2 3]
>     where eq a b = subst [(x1, l)] a `Equal` subst [(x2, 0)] b
>           x1 = prefix1 ++ "x"
>           x2 = prefix2 ++ "x"

Radial bearing
    y = R/j

> radialBearing desc coordinate r j rhs =
>     [(pv r newton / pv j (newton /. meter))
>      `Equal` subst [(x, pv coordinate meter)] rhs]
>     where x = prefix desc ++ "x"


Work with deflection line segments.

sectionDeflections returns list of (valid length, deflection function) pairs.
Deflection function is changed after each force or bearing applied.
Prefix is added to all variabled used to describe beam or bearing reaction,
i.e. A0...A3 become (prefix++A0...) and all reactions become prefixR1,2,...

> type SectionDeflections = (String, [(CASExpr, CASExpr)])

> sectionDeflections :: String -> Section -> SectionDeflections
> sectionDeflections prefix section =
>     (prefix,
>      sd (generalSolution desc)
>         1 -- start bearing number to use in prefixR1,2,...
>         0 -- start coordinate
>         (toList $ forces section) (toList $ bearings section))
>     where desc = (prefix,
>                   modulusOfElasticity (material section) /. pascal, -- E
>                   substitute (Map.fromList [("x", Symbol (prefix++"x"))]) $
>                       momentOfInertia section /. meter4) -- J
>           toList = map (\(c, a) -> (c /. meter, a)) . Map.toList
>                      -- we convert coordinates to undimensioned CASExpr
>           sd y bn c [] [] = [(sectionLength section /. meter - c, y)]
>           sd y bn c ((fc, f):fs) [] = addF y bn c fc f fs []
>           sd y bn c [] ((bc, b):bs) = addB y bn c bc b [] bs
>           sd y bn c ((fc, f):fs) ((bc, b):bs) =
>               if fc <= bc
>                  then addF y bn c fc f fs ((bc, b):bs)
>                  else addB y bn c bc b ((fc, f):fs) bs
>           addF y bn c fc f fs bs = (fc-c,y) : sd ynew bn fc fs bs
>               where ynew = if rf /= 0
>                            then ynew' + partialSolutionRadialForce desc rf fc
>                            else ynew'
>                     ynew' = if bm /= 0
>                            then y + partialSolutionBendingMoment desc bm fc
>                            else y
>                     rf = radialForce f /. newton
>                     bm = bendingMoment f /. (newton*.meter)
>           addB y bn c bc b fs bs =
>               (bc-c,y) -- length = bearing coordinate - start coordinate
>               : sd (y + partialSolutionRadialForce desc
>                     (Symbol $ prefix ++ "R" ++ show bn)
>                     -- ^ we don't know bearing reaction yet, it's our unknown
>                     bc) (bn+1) bc fs bs

> getSectionDeflection :: SectionDeflections -> CASExpr -> CASExpr
> getSectionDeflection (prefix, sd) c =
>     substitute (Map.fromList [(prefix++"x", (c/1000))]) $ leftmost sd (c/1000)

> type SpindleDeflections = [(CASExpr, (Section, SectionDeflections))]

> spindleDeflections :: Spindle -> SpindleDeflections
> spindleDeflections sp = spd 1 sp
>     where spd sn [] = []
>           spd sn (s:xs) = (sectionLength s /. meter,
>                            (s, sectionDeflections ("s"++show sn) s))
>                           : spd (sn+1) xs

> getSpindleDeflection :: SpindleDeflections -> CASExpr -> CASExpr
> getSpindleDeflection sd c = getSectionDeflection secd (coord*1000)
>     where ((sec, secd), coord) = leftmost' sd (c/1000)


Spindle equation system generation.

The equation system.

> spindleEquationSystem :: SpindleDeflections -> [CASExpr]
> spindleEquationSystem [] = []
> spindleEquationSystem (s:xs) =
>     freeEnd (desc s) (0::CASExpr) (leftmost3 s 0)
>     ++
>     equationSystem s xs
>   where -- description used in spindle equations (prefix, E, J)
>         desc (l, (section, (prefix, defls))) =
>             (prefix, modulusOfElasticity (material section) /. pascal,
>              momentOfInertia section /. meter4)
>             
>         sectionBoundaryConditions s@(l, (sec, (prefix, sd))) = concat $
>             map (\ ((c_m, bearing), bn) -> let c = c_m /. meter in
>                  radialBearing (desc s) c (Symbol $ prefix ++ "R" ++ show bn)
>                    (radialRigidity bearing) (rightmost sd c))
>                 $ zipWith (,) (Map.toList $ bearings sec) [1..]
>                   
>         equationSystem s [] = sectionBoundaryConditions s ++
>                               freeEnd (desc s) (l s) (rightmost3 s (l s))
>         equationSystem s (ns:xs) =
>             sectionBoundaryConditions s ++
>             connected (desc s) (rightmost3 s (l s)) (l s)
>                       (desc ns) (leftmost3 ns 0) 
>                 ++ equationSystem ns xs
>                    
>         rightmost3 s = rightmost (snd $ snd $ snd s)
>         leftmost3 s = leftmost (snd $ snd $ snd s)
>         l s = sectionLength (fst $ snd s) /. meter

Equation system unknowns.

> spindleEquationSystemUnknowns :: SpindleDeflections -> [String]
> spindleEquationSystemUnknowns [] = []
> spindleEquationSystemUnknowns ((l,(section,(prefix,sd))):xs) =
>     (map (prefix ++) $ ["A0", "A1", "A2", "A3"] ++
>          map (\ bn -> "R" ++ show bn) [1..Map.size (bearings section)])
>     ++
>     spindleEquationSystemUnknowns xs


Solving of spindle equation system using Maxima.

> solveSpindleDeflections :: Interpreter -> Spindle -> IO SpindleDeflections
> solveSpindleDeflections i s = do
>     let sd = spindleDeflections s
>     solution <- eval i $ solve (spindleEquationSystem sd)
>                 (spindleEquationSystemUnknowns sd)
>     --print solution
>     let substSolution =
>             case solution of
>                 List [List a] -> substitute (Map.fromList $ map toSubs a)
>                 _ -> error "solveSpindleDeflections: not solved"
>             where toSubs (Equal (Symbol s) e) = (s, e)
>                   toSubs _ = error "solveSpindleDeflections: invalid solution"
>     return $ map (\ (l, (s, (p, sd))) ->
>                   (l, (s, (p, map (\ (l,d) -> (l, substSolution d)) sd)))) sd

General utilities.

Leftmost value from section list.
Section list is a list of length-value pairs.

> leftmost :: [(CASExpr, a)] -> CASExpr -> a
> leftmost [] c = error "leftmost: coordinate is bigger than section list"
> leftmost ((l,a):xs) c = if l >= c then a else leftmost xs (c-l)

Leftmost section from section list & coordinate in returned section

> leftmost' :: [(CASExpr, a)] -> CASExpr -> (a, CASExpr)
> leftmost' [] c = error "leftmost': coordinate is bigger than section list"
> leftmost' ((l,a):xs) c = if l >= c then (a, c) else leftmost' xs (c-l)

Rightmost value from section list.

> rightmost :: [(CASExpr, a)] -> CASExpr -> a
> rightmost [] c = error "rightmost: coordinate is bigger than section list"
> rightmost ((l,a):x:xs) c = if l > c then a else rightmost (x:xs) (c-l)
> rightmost ((l,a):xs) c = if l >= c then a else rightmost xs (c-l)

Moment inertia of circle
    pi/64*d^4

> jCircle d = pi/64 * pv d meter**4

Area of circle.

> areaOfCircle d = pi/4 * pv d meter**2

substitutepv - same as CASExpr.substitute but for physical values

> substitutepv :: [(String, CASExpr)] -> Value a -> Value a
> substitutepv s (Value v) = Value (substitute (Map.fromList s) v)

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
>         --sj = jCircle (((110-100)*Symbol "x"/1000+100) .* mm)
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
>     let eqlist = (freeEnd desc c s ++
>                   freeEnd desc (a+.b) s2 ++
>                   radialBearing desc b (Symbol "R1") j s1 ++
>                   radialBearing desc (a+.b) (Symbol "R2") j s2)
>     r <- eval i $ solve eqlist ["A0", "A1", "A2", "A3", "R1", "R2"]
>     let List [a] = r
>     y0 <- eval i $ Funcall CFSubst [a, sf]
>     mapM_ (\ i -> do let y = (CASExpr.eval $
>                               substitute (Map.fromList [("x", i)]) y0)
>                              :: ExactNumber
>                      --printf "%.5f\n" $ y * (meter /. micro meter)))
>                      print $ y * CASExpr.eval (meter /. micro meter))
>               (map (* (mm /. meter)) [0,10..100])


The same as testCase1 but spindle is splitted in two sections.

> testCase1' = withInterpreter $ \i -> do
>     let d = 100 .* mm
>         sj = jCircle d
>         e = modulusOfElasticity steel /. pascal
>         desc1 = ("s1", e, sj)
>         desc2 = ("s2", e, sj)
>         c = 0 .* mm
>         b = 100 .* mm
>         a = 800 .* mm
>         j = 120 .* newton /. micro meter
>         --j = 12 .* kgf /. micro meter
>         s1 = generalSolution desc1
>         s2 = generalSolution desc2
>         s1f = s1  + partialSolutionRadialForce desc1 (1.*newton) c
>         s11 = s1f + partialSolutionRadialForce desc1 (Symbol "R1") b
>         s22 = s2  + partialSolutionRadialForce desc2 (Symbol "R2") (0.5.*a)
>     let eqlist = (freeEnd desc1 c s1 ++
>                   freeEnd desc2 (0.5.*(a+.b)) s22 ++
>                   connected desc1 s11 0.5 desc2 s2 ++
>                   radialBearing desc1 b (Symbol "R1") j s11 ++
>                   radialBearing desc2 (0.5.*a) (Symbol "R2") j s22)
>     r <- eval i $ solve eqlist ["s1A0", "s1A1", "s1A2", "s1A3",
>                                 "s2A0", "s2A1", "s2A2", "s2A3",
>                                 "R1", "R2"]
>     print r
>     let List [a] = r
>     y0 <- eval i $ Funcall CFSubst [a, s1f]
>     mapM_ (\ i -> do let y = (CASExpr.eval $
>                               substitute (Map.fromList [("s1x", i)]) y0)
>                              :: ExactNumber
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
>     let bearing = findBearingByCode "B7015C.T.P4S"
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
>     putStrLn (prefix ++ show (CASExpr.eval v {-:: ExactNumber -}))

> printSolvedVariable i prefix solution symbol = do
>     v <- substSolution i solution [] (Symbol symbol)
>     putStrLn (prefix ++ show (CASExpr.eval v :: ExactNumber))

printValues prints `function` results (in mum) for `parameter`
in `list` of values (in mm). `solution` is used to substitute all
other parameters

> printValues i solution function parameter list = do
>     f <- substSolution i solution [] function
>     mapM_ (\ i -> do let y = (CASExpr.eval $ substitute
>                               (Map.fromList [(parameter, i)]) $
>                               f * (meter /. micro meter)) :: ExactNumber
>                      putStrLn (show (truncate $ CASExpr.eval $
>                                      i .* meter /. mm)
>                                ++ "\t" ++ show y))
>               (map (* (mm /. meter)) list)


Test case #3.
The spindle of machine tool used in course work.
Console force of 1N and five FAG bearings.
Geometry is simplified against real spindle (many groves are removed),
but for deflection calculation it's OK.

> testCase3 = withInterpreter $ \i -> do
>     s <- solveSpindleDeflections i testSpindle
>     mapM_ (\ c -> print (CASExpr.eval (getSpindleDeflection s c)))
>           [0,100..400]

> testSpindle =
>     -- shaft
>     --makeRigid
>     ((cyl 82 13 <+> cyl 133 23 <+> cyl 120 8 <+> cyl 75 147.5
>       <+> cyl 67 90 <+> cyl 60 62.5 <+> cyl 57 96)
>      `cut`
>      (cyl 55 30 <+> cyl 45 73 <+> cyl 35 337))
>     -- forces
>     `addRadialForce` 1 `at` 0
>     -- front bearing set
>     `addBearing` fagB7015C `at` (44+27)
>     `addBearing` fagB7015C `at` (44+47)
>     `addBearing` fagB7015C `at` (44+79)
>     -- rear bearing set
>     `addBearing` fagB7012C `at` (281.5+31)
>     `addBearing` fagB7012C `at` (281.5+49)
>     -- end
>   where cyl = cylinder
>         fagB7015C = findBearingByCode "B7015C.T.P4S"
>         fagB7012C = findBearingByCode "B7012C.T.P4S"
>         makeRigid = map (\ s -> s { material = rigidMaterial })
>         rigidMaterial = IsotropicMaterial
>                         { modulusOfElasticity = (210000 * 10^6) .* mega pascal }

