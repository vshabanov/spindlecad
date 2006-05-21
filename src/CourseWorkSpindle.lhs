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

This module is temporary. Its used for manual course work spindle optimization.

> import CASExpr
> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList
> import Maxima hiding (eval)
> import ExactNumber
> import Bearing
> import Bearings.FAG.SpindleBearings
> import SpindleEquations

The spindle of machine tool used in course work.
Console force of 1N and five FAG bearings.
Geometry is simplified against real spindle (many groves are removed),
but for deflection calculation it's OK.

> all15 = filter
>     (contactAngle `is` (inexactEq (15*degree)))
>     bearingsList

> std15 = filter
>     (bearingType `is` (== "Standard bearing. Steel balls."))
>     all15

> for fun pred l f = mapM_ f (filter (fun `is` pred) l)


> testCase = withInterpreter $ \i -> do
>     for innerDiameter (== 75.*mm) std15 $ \ b1 -> do
>       for innerDiameter (== 60.*mm) std15 $ \ b2 -> do
>         --let b2 = findBearingByCode "B7012C.T.P4S"
>         s <- solveSpindleDeflections i (testSpindleConstructor b1 b2)
>         -- 32sec/16schemes = 2 sec per solveSpindleDeflections
>         tabbed 16 (code b1)
>         tabbed 16 (code b2)
>         print (eval (getSpindleDeflection s 0))

> is f predicate = predicate . f

> (&&&) f1 f2 x = f1 x && f2 x
> (|||) f1 f2 x = f1 x || f2 x

> infixl 8 `is`         -- same fixity as ^, but left associative
> infixr 3 &&&          -- &&
> infixr 2 |||          -- ||

> testSpindle = testSpindleConstructor fagB7015C fagB7012C
>   where fagB7015C = findBearingByCode "B7015C.T.P4S"
>         fagB7012C = findBearingByCode "B7012C.T.P4S"

> testSpindleConstructor b1 b2 =
>     -- shaft
>     --makeRigid
>     ((cyl 82 13 <+> cyl 133 23 <+> cyl 120 8
>       <+> cyl (innerDiameter b1 /. mm) (147.5+3*wd1)
>       <+> cyl 67 90
>       <+> cyl (innerDiameter b2 /. mm) (62.5+2*wd2)
>       <+> cyl 57 96)
>      `cut`
>      (cyl 55 30 <+> cyl 45 73 <+> cyl 35 (337+3*wd1+2*wd2)))
>     -- forces
>     `addRadialForce` 1 `at` 0
>     -- front bearing set
>     `addBearing` b1 `at` (44+27+0.5*wd1)
>     `addBearing` b1 `at` (44+47+1.5*wd1)
>     `addBearing` b1 `at` (44+79+2.5*wd1)
>     -- rear bearing set
>     `addBearing` b2 `at` (281.5+31+3*wd1+0.5*wd2)
>     `addBearing` b2 `at` (281.5+49+3*wd1+1.5*wd2)
>     -- end
>   where cyl = cylinder
>         wd1 = (width b1 -. width fagB7015C) /. mm
>         wd2 = (width b2 -. width fagB7012C) /. mm
>         fagB7015C = findBearingByCode "B7015C.T.P4S"
>         fagB7012C = findBearingByCode "B7012C.T.P4S"



> tabbed n s = do putStr s
>                 putStr $ (take (n - length s) $ repeat ' ') ++ " | "

-- > num n v = tabbed 8 s
-- >     where s = case n /. v of
-- >                   Integer i -> show i
-- >                   r -> show $ CASExpr.eval r

-- > reportBearingParams b = do
-- >     putStrLn "| Code                  | d     | D     | B     | k     | w1    | w1'   |"
-- >     putStrLn "|-----------------------+-------+-------+-------+-------+-------+-------+"
-- >     mapM_ (\ b -> do tabbed 3 $ "| " ++ code b
-- >                      num (innerDiameter b) mm
-- >                      num (outerDiameter b) mm
-- >                      num (width b) mm
-- >                      num (radialRigidity b) (newton /. micro meter)
-- >                      num (attainableSpeedGrease b) rpm
-- >                      num (0.65 .* attainableSpeedGrease b) rpm
-- >                      putStrLn "") b
-- >     putStrLn "|-----------------------+-------+-------+-------+-------+-------+-------+"

Example of use:
reportBearingParams $ filter (outerDiameter `is` (== 95.*mm) &&& contactAngle `is` (== 15*degree) &&& bearingType `is` (== "Standard bearing. Steel balls.")) bearingsList
