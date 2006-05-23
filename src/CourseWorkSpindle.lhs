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
> import qualified Maxima
> import ExactNumber
> import Bearing
> import Bearings.FAG.SpindleBearings
> import SpindleEquations
> import Text.Printf

The spindle of machine tool used in course work.
Console force of 1N and five FAG bearings.
Geometry is simplified against real spindle (many groves are removed),
but for deflection calculation it's OK.

> for fun pred l f = mapM_ f (filter (fun `is` pred) l)

> main = withInterpreter $ \i -> do
>     let sd = spindleDeflections testSpindle
>     --print sd
>     --print $ spindleEquationSystem sd
>     baseSsd <- solveSpindleDeflections i sd
>     let baseS = substdL 0 baseSsd
>     --print baseS
>     let baseLife = minimum $ map (\ (b, p, n) -> abs $ eval $ cdyn b /. n)
>                    (getBearingReactions baseS)
>         baseRigidity = rigidity baseS
>     for innerDiameter (>= 75.*mm) std15 $ \ b1 -> do
>       for innerDiameter (== 60.*mm) std15 $ \ b2 -> do
>         --let b2 = findBearingByCode "B7012C.T.P4S"
>         sd <- solveSpindle i (testSpindleConstructor b1 b2)
>         let sd0 = getSpindleDeflection sd (0.*mm)
>         -- dLopt <- Maxima.eval i $ solve [diff sd0 "dL" `Equal` 0] ["dL"]
>         -- diff sd0 has dL^6,dL^5, etc. maxima can only solve x^4...=0         
>         let (sd0opt, dLopt) = minimum $
>                               map (\ dl -> (abs $ eval $ substitutepv [("dL",dl/1000)] sd0
>                                             /. nano meter :: Double, dl/1000)) [-40..200]
>         let s = substdL 0 sd
>         let sopt = substdL dLopt sd
>         -- 32sec/16schemes = 2 sec per solveSpindleDeflections
>         -- 18sec/16schemes  when ghc -O
>         tabbed 9 (shortSode b1)
>         tabbed 9 (shortSode b2)
>         --printf "%5.3f | " $ deflection s
>         printf "%5.1f | " $ rigidity s
>         printf "%5.3f | " $ rigidity s / baseRigidity
>         printf "%5.2f | " $ relativeLife baseLife s
>         printf "%5.0f | " $ evald (dLopt*1000)
>         --printf "%5.3f | " $ deflection sopt
>         printf "%5.1f | " $ rigidity sopt
>         printf "%5.3f | " $ rigidity sopt / baseRigidity
>         printf "%5.2f | " $ relativeLife baseLife sopt
>         --mapM_ (\ (b, p, n) -> --tabbed 6 (take 6 $ show $
>         --                      printf "%6.2f | " (((abs $ (evald $ cdyn b/.n )) / baseLife)**3))
>         --          (getBearingReactions s)
>         putStrLn ""


Evaluation utilities.

> substdL dl = substituteSpindleDeflectionsParams [("dL",dl)]

> evald a = eval a :: Double

> relativeLifes baseLife s =
>     map (\ (b, p, n) -> ((abs $ (evald $ cdyn b/.n )) / baseLife)**3)
>             (getBearingReactions s)

> relativeLife baseLife s = minimum $ relativeLifes baseLife s

Console deflection, nano meter per 1 newton

> deflection s = (abs $ evald (getSpindleDeflection s (0.*mm) /. nano meter))

Spindle rigidity N/mum

> rigidity s = (abs $ evald ((1 ./ getSpindleDeflection s (0.*mm)) *. micro meter))


Bearing filtering utilities.

> all15 = filter
>     (contactAngle `is` (inexactEq (15*degree))
>      &&&
>      attainableSpeedGrease `is` (>= (7000/0.65).*rpm))
>     bearingsList

> std15 = filter
>     (bearingType `is` (== "Standard bearing. Steel balls."))
>     all15

> is f predicate = predicate . f

> (&&&) f1 f2 x = f1 x && f2 x
> (|||) f1 f2 x = f1 x || f2 x

> infixl 8 `is`         -- same fixity as ^, but left associative
> infixr 3 &&&          -- &&
> infixr 2 |||          -- ||

Output helpers.

> tabbed n s = do putStr s
>                 putStr $ (take (n - length s) $ repeat ' ') ++ " | "

> shortSode = takeWhile ((/=) '.') . code


Spindle description construction.

> testSpindle = testSpindleConstructor fagB7015C fagB7012C
>   where fagB7015C = findBearingByCode "B7015C.T.P4S"
>         fagB7012C = findBearingByCode "B7012C.T.P4S"

> testSpindleConstructor b1 b2 =
>     -- shaft
>     --makeRigid
>     modifySectionAt (\ s _ -> s { sectionLength = sectionLength s
>                                   +. Symbol "dL" .* meter 
>                                 })
>     (((cyl 82 13 <+> cyl 133 23 <+> cyl 120 8
>       -- <+> cyl (innerDiameter b1 /. mm) (147.5+3*wd1)
>       <+> cyl (innerDiameter b1 /. mm) 100
>       <+> cyl (innerDiameter b1 /. mm) (47.5+3*wd1) -- dL here
>       <+> cyl 67 90
>       <+> cyl (innerDiameter b2 /. mm) (62.5+2*wd2)
>       <+> cyl 57 96)
>      `cut`
>      (cyl 55 30 <+> cyl 45 73 <+> cyl 35 (337+3*wd1+2*wd2)))
>     -- forces
>     `addRadialForce` 1.*newton `at` 0.*mm
>     --`addBendingMoment` (0.2.*newton*.meter) `at` 1.*mm
>     -- front bearing set
>     `addBearing` b1 `at` (44+27+0.5*wd1).*mm
>     `addBearing` b1 `at` (44+47+1.5*wd1).*mm
>     `addBearing` b1 `at` (44+79+2.5*wd1).*mm
>     -- rear bearing set
>     `addBearing` b2 `at` (281.5+31+3*wd1+0.5*wd2).*mm
>     `addBearing` b2 `at` (281.5+49+3*wd1+1.5*wd2).*mm)
>     ((44+100 + 1).*mm) -- coordinate of section where dL is added to length
>     -- end
>   where cyl d l = cylinder (d.*mm) (l.*mm)
>         wd1 = (width b1 -. width fagB7015C) /. mm
>         wd2 = (width b2 -. width fagB7012C) /. mm
>         fagB7015C = findBearingByCode "B7015C.T.P4S"
>         fagB7012C = findBearingByCode "B7012C.T.P4S"



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
