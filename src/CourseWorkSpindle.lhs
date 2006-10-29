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

This module is temporary. Its used for manual course work spindle optimization.

> import TekHaskell
> import Bearing
> import Bearings.FAG.SpindleBearings
> import SpindleEquations
> import Text.Printf
> import Drawing
> import Data.IORef
> import qualified Data.Map as Map

The spindle of machine tool used in course work.
Console force of 1N and five FAG bearings.
Geometry is simplified against real spindle (many groves are removed),
but for deflection calculation it's OK.

> for fun pred l f = mapM_ f (filter (fun `is` pred) l)

> main = scanBearings --drawRunouts

> drawBaseSpindle = withInterpreter $ \i -> do
>     let sd = substpi $ spindleDeflections spindle
>         b1 = findBearingByCode "B7015C.T.P4S"--"B71917C.T.P4S""B71818C.TPA.P4"
>         b2 = findBearingByCode "B7012C.T.P4S"
>         spindle = testSpindleConstructor 1 0 b1 b2 [0,0..]
>     baseSsd <- solveSpindleDeflections i sd
>     let baseS = substdL 0 baseSsd
>     exportToACAD (substituteDrawing (Map.fromList [("dL",0)]) $
>                   spindleDrawing spindle) "C:/base-spindle.lsp"
>     exportToACAD (deflectionLine (10 .* mm /. nano meter) baseS)
>                  "C:/base-spindle-deflections.lsp"

> drawRunouts = withInterpreter $ \i -> do
>     let scaleDrawing = scale xscale yscale
>         xscale = 1/5
>         yscale = cm /. 5 .* micro meter
>         runoutsSpacing = 2.6 .* cm
>         b1 = findBearingByCode "B7015C.T.P4S"--"B71917C.T.P4S"
>         b2 = findBearingByCode "B7012C.T.P4S"
>         consoleLength = 300.*mm
>         spindleConstructor ro =
>             cylinder (100.*mm) consoleLength
>             <+> testSpindleConstructor 0 0 b1 b2 ro
>         generalSpindle = spindleConstructor runouts
>     putStr "solving spindle... "
>     generalSD <- solveSpindleDeflections i
>                  (substdL 0 $ substpi $ spindleDeflections $ generalSpindle)
>     putStrLn "done"
>     drawing <- newIORef (scale xscale xscale $
>                          substituteDrawing (Map.fromList [("dL",0)]) $
>                          spindleDrawing $ spindleConstructor [0,0..])
>     -- scan 16 runout schemes & accumulate its drawings in `drawing` IORef
>     flip mapM_ runoutSchemes $ \ (n,ro) -> do
>       let sd = substRunouts ro generalSD
>           spindle = spindleConstructor ro
>           runoutsDrawing =
>               move (0.*mm) ((-n).*runoutsSpacing -. 3.*cm) $
>               -- max deflection
>               centeredTextWithHeight (5.*mm) ((-2).*cm) (0.*mm)
>                    (printf "%.3f" (max deflection1 deflection2))
>               `over`
>               -- deflection at the end of part
>               Line ThinLine [Point (0.*mm) (0.5.*mm),
>                              Point (0.*mm) ((-0.5).*mm)]
>               `over`
>                centeredTextWithHeight (2.5.*mm) (0.*mm) ((-3).*mm)
>                    (printf "%.3f" deflection1)
>               `over`
>               -- deflection at the end of spindle
>               Line ThinLine [Point (xscale.*consoleLength) (0.5.*mm),
>                              Point (xscale.*consoleLength) ((-0.5).*mm)]
>               `over`
>               centeredTextWithHeight (2.5.*mm) (xscale.*consoleLength) ((-3).*mm)
>                   (printf "%.3f" deflection2)
>               `over`
>               -- 
>               scaleDrawing (deflectionLine 1 sd `over` runoutsScheme spindle)
>           deflection1 = deflectionAt (0.*mm) sd / 1000
>           deflection2 = deflectionAt consoleLength sd / 1000
>       modifyIORef drawing (runoutsDrawing `over`)
>       print (eval n, map eval ro, map truncate $ relativeLives 1 sd)
>     -- export the drawing
>     putStr "exporting drawing... "
>     finalDrawing <- readIORef drawing
>     exportToACAD finalDrawing "c:/runouts.lsp"
>     putStrLn "done"

> scanBearings = withInterpreter $ \i -> do
>     baseSsd <- solveSpindleDeflections i (substpi $ spindleDeflections testSpindle)
>     let baseS = substdL 0 baseSsd
>     let baseLife = minimum $ relativeLives 1 baseS
>         baseRigidity = rigidity baseS
>     --print $ relativeLives 1 baseS
>     --print ("Base life", baseLife)
>     --for innerDiameter (>= 75.*mm) [findBearingByCode "B7015C.T.P4S"] $ \ b1 -> do
>     for innerDiameter (>= 75.*mm) std15 $ \ b1 -> do
>       for innerDiameter (== 60.*mm) std15 $ \ b2 -> do
>         --let b2 = findBearingByCode "B7012C.T.P4S"
>         sd <- solveSpindleDeflections i
>               (substpi $ spindleDeflections $ testSpindleConstructor 1 0 b1 b2 [0,0..])
>         let sd0 = getSpindleDeflection sd (0.*mm)
>         -- dLopt <- Maxima.eval i $ solve [diff sd0 "dL" `Equal` 0] ["dL"]
>         -- diff sd0 has dL^6,dL^5, etc. maxima can only solve x^4...=0         
>         let (sd0opt, dLopt) = minimum $
>                               map (\ dl -> (abs $ eval $ substitutepv' [("dL",dl/1000)] sd0
>                                             /. nano meter :: Double, dl/1000)) [-40..500]
>         let s = substdL 0 sd
>         let sopt = substdL dLopt sd
>         -- 32sec/16schemes = 2 sec per solveSpindleDeflections
>         -- 18sec/16schemes  when ghc -O
>         tabbed 9 (shortCode b1)
>         tabbed 9 (shortCode b2)
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
>         --exportToACAD (deflectionLine (10 .* mm /. nano meter) s)
>         --         ("c:/" ++ shortCode b1 ++ "-" ++ shortCode b2 ++ ".lsp")
>         --exportToACAD (deflectionLine (10 .* mm /. nano meter) sopt)
>         --         ("c:/" ++ shortCode b1 ++ "-" ++ shortCode b2 ++ "-opt.lsp")
>         -- Calculate optimal runouts scheme --
>         let consoleLength = 300.*mm
>             spindleConstructor ro =
>                 cylinder (100.*mm) consoleLength
>                 <+> testSpindleConstructor 0 0 b1 b2 ro
>         sdr <- solveSpindleDeflections i
>                (substdL 0 $ substpi $ spindleDeflections $
>                 spindleConstructor runouts)
>         let runouts = map (\ (n, ro) ->
>                            let sd = substRunouts ro sdr
>                                deflection1 = deflectionAt (0.*mm) sd / 1000
>                                deflection2 = deflectionAt consoleLength sd / 1000 in
>                            (max deflection1 deflection2, ro))
>                           runoutSchemes
>             (minRunout, optRunouts) = minimum runouts
>         printf "%5.3f | " minRunout
>         putStr (map (\ d -> if d == 1 then '+' else '-') optRunouts)
>         --               
>         putStrLn ""

Evaluation utilities.

> runouts = [1,
>            Symbol "roa",
>            Symbol "rob",
>            Symbol "roc",
>            Symbol "rod"]

> runoutSchemes = [(n,[1,a,b,c,d])
>                  | n <- [0..]
>                  | a <- [1,-1], b <- [1,-1], c <- [1,-1], d <- [1,-1]]

> substRunouts [1, a, b, c, d] =
>     substituteSpindleDeflectionsParams [("roa", a),
>                                         ("rob", b),
>                                         ("roc", c),
>                                         ("rod", d)]

> substdL dl = substituteSpindleDeflectionsParams [("dL",dl)]

> substpi = substituteSpindleDeflectionsParams [("_cas_pi",Rational $ toRational pi)]

> evald a = eval a :: Double

> relativeLives baseLife s =
>     map (\ (b, pos, r) -> (((evald $ cdyn b /. newton)
>                             /
>                             (abs $ evald $ r /. newton))**3 * 10^6 / (60*7000))
>                            / baseLife)
>             (getBearingReactions s)

> relativeLife baseLife s = minimum $ relativeLives baseLife s

Console deflection, nano meter per 1 newton

> deflection s = (abs $ evald (getSpindleDeflection s (0.*mm) /. nano meter))

> deflectionAt pos s = (abs $ evald (getSpindleDeflection s pos /. nano meter))

Spindle rigidity N/mum

> rigidity s = (abs $ evald ((1 ./ getSpindleDeflection s (0.*mm)) *. micro meter))

Deflection line drawing

> deflectionLine scale s = Spline NormalLine $
>                          map (\ (x, y) -> Point x (scale.*y)) deflections
>     where deflections = getSpindleDeflections s [0.*mm, 1.*mm ..
>                                                  getSpindleDeflectionsLength s]

Runouts scheme drawing

> runoutsScheme spindle = changeLineStyleTo ThinLine $
>                         substituteDrawing (Map.fromList [("dL",0)]) $
>                                           runoutsSchemeDrawing spindle

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

> shortCode = takeWhile ((/=) '.') . code


Spindle description construction.

> testSpindle = testSpindleConstructor 1 0 fagB7015C fagB7012C [0,0,0,0,0]
>   where fagB7015C = findBearingByCode "B7015C.T.P4S"
>         fagB7012C = findBearingByCode "B7012C.T.P4S"

> testSpindleConstructor f l b1 b2 ro =
>     -- shaft
>     (((cyl 82 13 <+> cyl 133 23 <+> cyl 120 8
>       -- <+> cyl (innerDiameter b1 /. mm) (147.5+3*wd1)
>       <+> cyl (innerDiameter b1 /. mm) (100+3*wd1)
>       <+> cyl (innerDiameter b1 /. mm) (47.5) -- dL here
>       <+> cyl 67 90
>       <+> cyl (innerDiameter b2 /. mm) (62.5+2*wd2)
>       <+> cyl 57 96)
>      `cut`
>      (cyl 55 30 <+> cyl 45 73 <+> cyl 35 (337+3*wd1+2*wd2)))
>     -- forces
>     `modify` (if f == 0 then (\s _ -> s) else addRadialForce (f.*newton)) `at` 0.*mm
>     `modify` (if l == 0 then (\s _ -> s) else addBendingMoment ((f*l).*newton*.meter)) `at` 1.*mm
>     -- front bearing set
>     `modify` addBearing' b1 MountLeft  (ro!!0) `at` (44+27+0.5*wd1).*mm
>     `modify` addBearing' b1 MountLeft  (ro!!1) `at` (44+47+1.5*wd1).*mm
>     `modify` addBearing' b1 MountRight (ro!!2) `at` (44+79+2.5*wd1).*mm
>     -- rear bearing set
>     `modify` addBearing' b2 MountLeft  (ro!!3) `at` (281.5+31+3*wd1+0.5*wd2).*mm
>     `modify` addBearing' b2 MountRight (ro!!4) `at` (281.5+49+3*wd1+1.5*wd2).*mm)
>     -- section where dL is added to length
>     `modify` (\ s _ -> s { sectionLength = sectionLength s
>                                   +. Symbol "dL" .* meter 
>                                 })
>              `at` ((44+100+3*wd1+1).*mm) 
>     -- end
>   where cyl d l = cylinder (d.*mm) (l.*mm)
>         wd1 = (width b1 -. width fagB7015C) /. mm
>         wd2 = (width b2 -. width fagB7012C) /. mm
>         fagB7015C = findBearingByCode "B7015C.T.P4S"
>         fagB7012C = findBearingByCode "B7012C.T.P4S"



> num n v = tabbed 5 s
>     where s = case n /. v of
>                   Integer i -> show i
>                   r -> show $ eval r

> reportBearingParams b = do
>     putStrLn "| Code                  | d     | D     | B     | k     | Cdyn  | w1    | w1'   |"
>     putStrLn "|-----------------------+-------+-------+-------+-------+-------+-------+-------+"
>     mapM_ (\ b -> do tabbed 23 $ "| " ++ code b
>                      num (innerDiameter b) mm
>                      num (outerDiameter b) mm
>                      num (width b) mm
>                      num (radialRigidity b) (newton /. micro meter)
>                      num (cdyn b) kN
>                      num (attainableSpeedGrease b) rpm
>                      num (0.65 .* attainableSpeedGrease b) rpm
>                      putStrLn "") b
>     putStrLn "|-----------------------+-------+-------+-------+-------+-------+-------+-------+"

Example of use:
reportBearingParams $ filter (outerDiameter `is` (== 95.*mm) &&& contactAngle `is` (== 15*degree) &&& bearingType `is` (== "Standard bearing. Steel balls.")) bearingsList
