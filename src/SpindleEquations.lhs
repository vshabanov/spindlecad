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
--  along with SpindleCAD; if not, write to the Free Software
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
> import Drawing


Spindle description data type.

> type Spindle = [Section]
>     
> data Section = Section { momentOfInertia :: Value Meter4,
>                          material :: Material,
>                          sectionLength :: Value Meter,
>                          sectionDrawing :: Drawing,
>                          forces :: Map.Map (Value Meter) Force,
>                          bearings :: Map.Map (Value Meter)
>                                              (MountScheme, Bearing)
>                        }
>                deriving (Eq, Ord, Show)
>                         
> data Force = Force { radialForce :: Value Newton,
>                      bendingMoment :: Value NewtonMulMeter 
>                    }
>              deriving (Eq, Ord, Show)
>                       
> data MountScheme = MountScheme { mountDirection :: MountDirection,
>                                  innerRingRadialRunoutCoefficient :: CASExpr
>                                }
>                    deriving (Eq, Ord, Show)
>
> data MountDirection = MountLeft
>                     | MountRight
>                       deriving (Eq, Ord, Show)


Spindle construction functions.
Steel is default material.

Cylindrical section description.

> cylinder :: Value Meter -> Value Meter -> Spindle
> cylinder d l = [Section { momentOfInertia = jCircle d,
>                           material = steel,
>                           sectionLength = l,
>                           sectionDrawing =
>                             Line NormalLine [Point (0.*mm) ((1/2).*d),
>                                              Point l ((1/2).*d),
>                                              Point l ((-1/2).*d),
>                                              Point (0.*mm) ((-1/2).*d),
>                                              Point (0.*mm) ((1/2).*d)],
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

Spindle description modification.
Usage: spindle `modify` addSomething ... `at` ... .* mm
               ...
               `modify` addSomething ... `at` ... .* mm 

> modify :: Spindle -> ((Section -> Value Meter -> Section), Value Meter)
>        -> Spindle
> modify s (f, c) = modifySectionAt f s c

Something `at` specified coordinate description.

> at :: a -> Value Meter -> (a, Value Meter)
> at a b = (a, b)

Radial force description.

> addRadialForce :: Value Newton -> (Section -> Value Meter -> Section)
> addRadialForce force = addrf
>     where addrf s c = s { forces = Map.insert c
>                           (Force { radialForce = force,
>                                    bendingMoment = 0 .* newton *. meter })
>                           (forces s) }

Bending moment description.

> addBendingMoment :: Value NewtonMulMeter
>                  -> (Section -> Value Meter -> Section)
> addBendingMoment moment = addbm
>     where addbm s c = s { forces = Map.insert c
>                           (Force { radialForce = 0 .* newton,
>                                    bendingMoment = moment })
>                           (forces s) }

Bearing description.

> defaultMountScheme = MountScheme { mountDirection = MountLeft,
>                                    innerRingRadialRunoutCoefficient = 0 
>                                  }

> addBearing :: Bearing -> (Section -> Value Meter -> Section)
> addBearing bearing = addb
>     where addb s c =
>               s { bearings = Map.insert c (defaultMountScheme, bearing)
>                              (bearings s) }

> addBearing' :: Bearing -> MountDirection -> CASExpr
>             -> (Section -> Value Meter -> Section)
> addBearing' bearing md irrrc = addb
>     where addb s c =
>               s { bearings = Map.insert c
>                   (MountScheme { mountDirection = md,
>                                  innerRingRadialRunoutCoefficient = irrrc
>                                },
>                    bearing)
>                   (bearings s) }

Sequential spindle connection.

> (<+>) :: Spindle -> Spindle -> Spindle
> (<+>) = (++)

Spindle bore description. The bore is just another spindle of equal length
which is cut from base spindle.
The cut part is drawn using HiddenLine.

> cut :: Spindle -> Spindle -> Spindle
> cut = unionSpindles cutSection
>     where cutSection base bore =
>               base { momentOfInertia =
>                      momentOfInertia base -. momentOfInertia bore,
>                      sectionDrawing = sectionDrawing base `over`
>                                       changeLineStyleTo HiddenLine 
>                                         (sectionDrawing bore)
>                    }

Makes spindle shaft rigidity millon times greater than those steel,
i.e. makes shaft practically absolutely rigid.
TODO: This function was initially indroduced to determine what part of
deflection is shaft deflection and what is bearings deflection.
But this is incorrect method for multibearing (and for two bearing?) shaft.
I think that something like least squares method on bearing deflections
can give more precice bearing deflection than absolutely rigid shaft.

> makeShaftRigid = map (\ s -> s { material = rigidMaterial })
>     where rigidMaterial = steel { modulusOfElasticity =
>                                   (10^6) .* modulusOfElasticity steel }

Fixities for construction functions and operators

> infixl 6 `at`                 -- same fixity as +
> infixr 6 <+>                  -- same fixity as +, but right associative
> infixl 5 `cut`                -- same fixity as ++, but left associative
> infixl 5 `modify`             -- same fixity as ++, but left associative


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
>           u [] a  = error "unionSpindles: spinle1 is shorter than spindle2"
>           u a  [] = error "unionSpindles: spinle2 is shorter than spindle1"
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
sectionDrawing is left in the left section

> splitSection :: Section -> Value Meter -> (Section, Section)
> splitSection s l =
>     if sectionLength s < l
>        then error "splitSection: section is shorter than specified coordinate"
>        else (s { sectionLength = l,
>                  forces = Map.filterWithKey   (\ k _ -> k <= l) $ forces s,
>                  bearings = Map.filterWithKey (\ k _ -> k <= l) $ bearings s
>                },
>              s { momentOfInertia = substitutepv' [("x", Symbol "x" + l/.meter)]
>                    (momentOfInertia s),
>                  sectionLength = sectionLength s -. l,
>                  sectionDrawing = EmptyDrawing,
>                  forces =   Map.mapKeys (\ k -> k -. l) $
>                    Map.filterWithKey (\ k _ -> k > l) $ forces s,
>                  bearings = Map.mapKeys (\ k -> k -. l) $
>                    Map.filterWithKey (\ k _ -> k > l) $ bearings s
>                })


Querying properties of described spindle.

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

> type Desc = (String,          -- prefix
>              Value Pascal,    -- E
>              Value Meter4)    -- J

> generalSolution :: Desc -> Value Meter
> generalSolution desc =
>     divEJ desc $ (a0 +. a1.*x +. a2*.square x +. a3*.cube x)
>               *. (newton *. meter2) -- E*J dimension
>     where s = symbol desc
>           x = s "x" .* meter
>           a0 = s "A0" .* meter
>           a1 = s "A1"
>           a2 = s "A2" ./ meter
>           a3 = s "A3" ./ meter2

> divEJ  (prefix, e, j) = (/. (e *. j))
> symbol (prefix, e, j) a = Symbol (prefix ++ a)
> prefix (p, _, _) = p


Partial solutions.
They are added to deflection (y) at the point where load is changed.
E.g. y_new = y + partialSolution

Radial force
    -P/6*(x-a)^3
    ------------
        E*J

> partialSolutionRadialForce :: Desc -> Value Newton -> Value Meter
>                            -> Value Meter
> partialSolutionRadialForce desc force coordinate =
>     divEJ desc $ (-1/6) .* force *. cube (x -. coordinate)
>     where s = symbol desc
>           x = s "x" .* meter

Bending moment
    -M/2*(x-a)^2
    ------------
        E*J

> partialSolutionBendingMoment :: Desc -> Value NewtonMulMeter -> Value Meter
>                                 -> Value Meter
> partialSolutionBendingMoment desc moment coordinate =
>     divEJ desc $ (-1/2) .* moment *. square (x -. coordinate)
>     where s = symbol desc
>           x = s "x" .* meter


Boundary conditions.
  
Free end.
    y'' = 0, y''' = 0

> freeEnd :: Desc -> Value Meter -> Value Meter -> [CASExpr]
> freeEnd desc coordinate rhs =
>     [subst [(x, coordinate /. meter)] (diffn (rhs/.meter) x 2) `Equal` 0,
>      subst [(x, coordinate /. meter)] (diffn (rhs/.meter) x 3) `Equal` 0]
>     where x = prefix desc ++ "x"

Connected sections.
    y1(l)  = y2(0)                      - deflection equality
    y1'(l) = y2'(0)                     - angles equality
    y1''(l)*E1*J1 = y2''(0)*E2*J2       - moments equality
    y1'''(l)*E1*J1 = y2'''(0)*E2*J2     - forces equality

> connected :: Desc -> Value Meter -> Value Meter -> Desc -> Value Meter
>           -> [CASExpr]
> connected (prefix1,e1,j1) y1 l (prefix2,e2,j2) y2 =
>     [(y1/.meter) `eq` (y2/.meter),
>      diff (y1/.meter) x1 `eq` diff (y2/.meter) x2,
>      diffn' (y1*.e1*.j1) x1 2 `eq` diffn' (y2*.e2*.j2) x2 2,
>      diffn' (y1*.e1*.j1) x1 3 `eq` diffn' (y2*.e2*.j2) x2 3]
>     where eq a b = subst [(x1, l/.meter)] a `Equal` subst [(x2, 0)] b
>           x1 = prefix1 ++ "x"
>           x2 = prefix2 ++ "x"
>           diffn' a = diffn (a/.(newton*.meter3))

Radial bearing
    y = R/j + runout

> radialBearing :: Desc -> Value Meter -> Value Newton -> Value NewtonDivMeter
>                  -> Value Meter -> Value Meter -> [CASExpr]
> radialBearing desc coordinate r j runout rhs =
>     [(r /. j +. runout) /. meter
>      `Equal` subst [(x, coordinate /. meter)] (rhs/.meter)]
>     where x = prefix desc ++ "x"


Work with deflection line segments.

sectionDeflections returns list of (valid length, deflection function) pairs.
Deflection function is changed after each force or bearing applied.
Prefix is added to all variabled used to describe beam or bearing reaction,
i.e. A0...A3 become (prefix++A0...) and all reactions become prefixR1,2,...

> type SectionDeflections = (String,          -- section prefix
>                            [(Value Meter,   -- sub-section length
>                              Value Meter)]) -- deflection function

> sectionDeflections :: String -> Section -> SectionDeflections
> sectionDeflections prefix section =
>     (prefix,
>      sd (generalSolution desc)
>         1 -- start bearing number to use in prefixR1,2,...
>         (0.*meter) -- start coordinate
>         (toList $ forces section) (toList $ bearings section))
>     where desc = (prefix,
>                   modulusOfElasticity (material section), -- E
>                   substitutepv' [("x", Symbol (prefix++"x"))] $
>                       momentOfInertia section) -- J
>           toList = map (\(c, a) -> (c, a)) . Map.toList
>                      -- we convert coordinates to undimensioned CASExpr
>           sd y bn c [] [] = [(sectionLength section -. c, y)]
>           sd y bn c ((fc, f):fs) [] = addF y bn c fc f fs []
>           sd y bn c [] ((bc, b):bs) = addB y bn c bc b [] bs
>           sd y bn c ((fc, f):fs) ((bc, b):bs) =
>               if fc <= bc
>                  then addF y bn c fc f fs ((bc, b):bs)
>                  else addB y bn c bc b ((fc, f):fs) bs
>           addF y bn c fc f fs bs = (fc-.c,y) : sd ynew bn fc fs bs
>               where ynew = if rf /= 0.*newton
>                            then ynew' +. partialSolutionRadialForce desc rf fc
>                            else ynew'
>                     ynew' = if bm /= 0.*newton*.meter
>                            then y +. partialSolutionBendingMoment desc bm fc
>                            else y
>                     rf = radialForce f
>                     bm = bendingMoment f
>           addB y bn c bc b fs bs =
>               (bc-.c,y) -- length = bearing coordinate - start coordinate
>               : sd (y +. partialSolutionRadialForce desc
>                     (Symbol (prefix ++ "R" ++ show bn) .* newton)
>                     -- ^ we don't know bearing reaction yet, it's our unknown
>                     bc) (bn+1) bc fs bs

> getSectionDeflection :: SectionDeflections -> Value Meter -> Value Meter
> getSectionDeflection (prefix, sd) c =
>     substitutepv' [(prefix++"x", c/.meter)] $ leftmost sd c

> type SpindleDeflections = [(Value Meter, (Section, SectionDeflections))]

> spindleDeflections :: Spindle -> SpindleDeflections
> spindleDeflections sp = spd 1 sp
>     where spd sn [] = []
>           spd sn (s:xs) = (sectionLength s,
>                            (s, sectionDeflections ("s"++show sn) s))
>                           : spd (sn+1) xs

> getSpindleDeflection :: SpindleDeflections -> Value Meter -> Value Meter
> getSpindleDeflection sd c = getSectionDeflection secd coord
>     where ((sec, secd), coord) = leftmost' sd c

> getSpindleDeflections :: SpindleDeflections -> [Value Meter]
>                       -> [(Value Meter, Value Meter)]
> getSpindleDeflections sd coords =
>     map (\ c -> (c, getSpindleDeflection sd c)) coords

> getSpindleDeflectionsLength :: SpindleDeflections -> Value Meter
> getSpindleDeflectionsLength s = l (0.*meter) s
>     where l acc [] = acc
>           l acc ((len, _):xs) = l (acc +. len) xs

> getBearingReactions :: SpindleDeflections
>                     -> [(Bearing, Value Meter, Value Newton)]
> getBearingReactions sd = br (0.*mm) sd
>     where br l [] = []
>           br l ((len, (sec, defls)):xs) =
>               map (\ (pos, (ms,b)) -> (b, l+.pos,
>                                        -- R=y*j
>                                        getSectionDeflection defls pos *.
>                                        radialRigidity b))
>                       (Map.toAscList $ bearings sec)
>               ++
>               br (l +. len) xs

Spindle equation system generation.

The equation system.

> spindleEquationSystem :: SpindleDeflections -> [CASExpr]
> spindleEquationSystem [] = []
> spindleEquationSystem (s:xs) =
>     freeEnd (desc s) (0.*mm) (leftmost3' s)
>     ++
>     equationSystem s xs
>   where -- description used in spindle equations (prefix, E, J)
>         desc (l, (section, (prefix, defls))) =
>             (prefix, modulusOfElasticity (material section),
>              momentOfInertia section)
>             
>         sectionBoundaryConditions s@(l, (sec, (prefix, sd))) = concat $
>             map (\ ((c, (mountScheme, bearing)), bn) ->
>                  radialBearing (desc s) c
>                    (Symbol (prefix ++ "R" ++ show bn) .* newton)
>                    (radialRigidity bearing)
>                    (innerRingRadialRunoutCoefficient mountScheme .*
>                     innerRingRadialRunout bearing)
>                    (rightmost sd c))
>                 $ zipWith (,) (Map.toList $ bearings sec) [1..]
>                   
>         equationSystem s [] = sectionBoundaryConditions s ++
>                               freeEnd (desc s) (l s) (rightmost3' s)
>         equationSystem s (ns:xs) =
>             sectionBoundaryConditions s ++
>             connected (desc s) (rightmost3' s) (l s)
>                       (desc ns) (leftmost3' ns) 
>                 ++ equationSystem ns xs
>                    
>         rightmost3' s = snd $ last $ snd $ snd $ snd s
>         leftmost3'  s = snd $ head $ snd $ snd $ snd s
>         l s = sectionLength (fst $ snd s)

Equation system unknowns.

> spindleEquationSystemUnknowns :: SpindleDeflections -> [String]
> spindleEquationSystemUnknowns [] = []
> spindleEquationSystemUnknowns ((l,(section,(prefix,sd))):xs) =
>     (map (prefix ++) $ ["A0", "A1", "A2", "A3"] ++
>          map (\ bn -> "R" ++ show bn) [1..Map.size (bearings section)])
>     ++
>     spindleEquationSystemUnknowns xs


Solving of spindle equation system using Maxima.

> solveSpindle :: Interpreter -> Spindle -> IO SpindleDeflections
> solveSpindle i s = solveSpindleDeflections i (spindleDeflections s)

> solveSpindleDeflections :: Interpreter -> SpindleDeflections
>                         -> IO SpindleDeflections
> solveSpindleDeflections i sd = do
>     solution <- eval i $ solve (spindleEquationSystem sd)
>                 (spindleEquationSystemUnknowns sd)
>     --print solution
>     let substSolution =
>             case solution of
>                 List [List a] -> map toSubs a
>                 _ -> error "solveSpindleDeflections: not solved"
>             where toSubs (Equal (Symbol s) e) = (s, e)
>                   toSubs _ = error "solveSpindleDeflections: invalid solution"
>     return $ substituteSpindleDeflectionsParams substSolution sd

> substituteSpindleDeflectionsParams :: [(String, CASExpr)]
>                                    -> SpindleDeflections -> SpindleDeflections
> substituteSpindleDeflectionsParams kvl sd =
>     let m = Map.fromList kvl
>         subst = substitutepv m
>         substd = substituteDrawing m in
>     map (\ (l, (s, (p, secd))) ->
>          (subst l,
>           (s { momentOfInertia = subst $ momentOfInertia s,
>                sectionLength = subst $ sectionLength s,
>                sectionDrawing = substd $ sectionDrawing s,
>                forces = Map.map
>                  (\ f -> Force { radialForce = subst $ radialForce f,
>                                  bendingMoment = subst $ bendingMoment f 
>                                }) $ forces s
>              },
>            (p,
>             map (\ (l,d) -> (subst l, subst d)) secd))))
>     sd

General utilities.

> spindleDrawing :: Spindle -> Drawing
> spindleDrawing s = d (0.*mm) s
>     where d l [] = EmptyDrawing
>           d l (s:xs) =
>               (move l (0.*mm) $ sectionDrawing s)
>               `over`
>               (foldl over EmptyDrawing $
>                map (\ (p, (ms, b)) -> move (l+.p) (0.*mm) $
>                     if mountDirection ms == MountLeft
>                       then bearingDrawing b
>                       else mirrorY $ bearingDrawing b) $
>                Map.toAscList $ bearings s)
>               `over`
>               d (l +. sectionLength s) xs

Leftmost value from section list.
Section list is a list of length-value pairs.

> leftmost :: [(Value Meter, a)] -> Value Meter -> a
> leftmost [] c = error "leftmost: coordinate is bigger than section list"
> leftmost ((l,a):xs) c = if l >= c then a else leftmost xs (c-.l)

Leftmost section from section list & coordinate in returned section

> leftmost' :: [(Value Meter, a)] -> Value Meter -> (a, Value Meter)
> leftmost' [] c = error "leftmost': coordinate is bigger than section list"
> leftmost' ((l,a):xs) c = if l >= c then (a, c) else leftmost' xs (c-.l)

Rightmost value from section list.

> rightmost :: [(Value Meter, a)] -> Value Meter -> a
> rightmost [] c = error "rightmost: coordinate is bigger than section list"
> rightmost ((l,a):x:xs) c = if l > c then a else rightmost (x:xs) (c-.l)
> rightmost ((l,a):xs) c = if l >= c then a else rightmost xs (c-.l)

Moment inertia of circle
    pi/64*d^4

> jCircle :: Value Meter -> Value Meter4
> jCircle d = (pi/64) .* fourth d

Area of circle.

> areaOfCircle :: Value Meter -> Value Meter2
> areaOfCircle d = (pi/4) .* square d

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
>         r1 = Symbol "R1" .* newton
>         r2 = Symbol "R2" .* newton
>         sf = s  +. partialSolutionRadialForce desc (1.*newton) c
>         s1 = sf +. partialSolutionRadialForce desc r1 b
>         s2 = s1 +. partialSolutionRadialForce desc r2 (a+.b)
>     let eqlist = (freeEnd desc c s ++
>                   freeEnd desc (a+.b) s2 ++
>                   radialBearing desc b r1 j (0.*mm) s1 ++
>                   radialBearing desc (a+.b) r2 j (0.*mm) s2)
>     r <- eval i $ solve eqlist ["A0", "A1", "A2", "A3", "R1", "R2"]
>     let List [a] = r
>     y0 <- eval i $ Funcall CFSubst [a, sf/.meter]
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
>         e = modulusOfElasticity steel
>         desc1 = ("s1", e, sj)
>         desc2 = ("s2", e, sj)
>         c = 0 .* mm
>         b = 100 .* mm
>         a = 800 .* mm
>         j = 120 .* newton /. micro meter
>         --j = 12 .* kgf /. micro meter
>         s1 = generalSolution desc1
>         s2 = generalSolution desc2
>         r1 = Symbol "R1" .* newton
>         r2 = Symbol "R2" .* newton
>         s1f = s1  +. partialSolutionRadialForce desc1 (1.*newton) c
>         s11 = s1f +. partialSolutionRadialForce desc1 r1 b
>         s22 = s2  +. partialSolutionRadialForce desc2 r2 (0.5.*a)
>     let eqlist = (freeEnd desc1 c s1 ++
>                   freeEnd desc2 (0.5.*(a+.b)) s22 ++
>                   connected desc1 s11 (500.*mm) desc2 s2 ++
>                   radialBearing desc1 b r1 j (0.*mm) s11 ++
>                   radialBearing desc2 (0.5.*a) r2 j (0.*mm) s22)
>     r <- eval i $ solve eqlist ["s1A0", "s1A1", "s1A2", "s1A3",
>                                 "s2A0", "s2A1", "s2A2", "s2A3",
>                                 "R1", "R2"]
>     print r
>     let List [a] = r
>     y0 <- eval i $ Funcall CFSubst [a, s1f/.meter]
>     mapM_ (\ i -> do let y = (CASExpr.eval $
>                               substitute (Map.fromList [("s1x", i)]) y0)
>                              :: ExactNumber
>                      --printf "%.5f\n" $ y * (meter /. micro meter)))
>                      print $ y * CASExpr.eval (meter /. micro meter))
>               (map (* (mm /. meter)) [0,10..100])
