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
Also it used for some other experiments.

> import TekHaskell
> import Bearing
> import Bearings.FAG.SpindleBearings
> import SpindleEquations
> import Text.Printf
> import Drawing
> import Data.IORef
> import qualified Data.Map as Map
> import qualified Data.Set as Set

The spindle of machine tool used in course work.
Console force of 1N (or none) and five FAG bearings.
Geometry is simplified against real spindle (many groves are removed),
but for deflection calculation it's OK.

> for fun pred l f = mapM_ f (filter (fun `is` pred) l)

> main = testSeparateInnerRingRotation --scanBearings --drawRunouts

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

> runoutSchemes = [(n,[1,b,c,d,e])
>                  | n <- [0..]
>                  | b <- [1,-1], c <- [1,-1], d <- [1,-1], e <- [1,-1]]

> drawRunouts = withInterpreter $ \i -> do
>     let scaleDrawing = scale xscale yscale
>         xscale = 1/5
>         yscale = cm /. 5 .* micro meter
>         runoutsSpacing = 2.6 .* cm
>         b1 = findBearingByCode "B7015C.T.P4S"--"B71917C.T.P4S"
>         b2 = findBearingByCode "B7012C.T.P4S"
>         spindleConstructor ro = console <+> testSpindleConstructor 0 0 b1 b2 ro
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

Experiment: rotating shaft with runouts

> data Vector = Vector { x :: CASExpr,
>                        y :: CASExpr 
>                      }

> addVectors v1 v2 = Vector { x = x v1 + x v2,
>                             y = y v1 + y v2 
>                           }

> addVectorLists v1 v2 = map (\(a,b) -> addVectors a b) $ zip v1 v2

> rotateVector a v = Vector { x = x v * c - y v * s,
>                             y = x v * s + y v * c
>                           }
>     where c = cos a
>           s = sin a

> vectorLength v = sqrt $ x v ^ 2 + y v ^ 2

> tupleToVector (x,y) = Vector { x = x, y = y }
> vectorToPoint v = Point ((scale * x v) .* micro meter)
>                         ((scale * y v) .* micro meter)
>     where scale = cm /. micro meter
> vectorToEvaluatedTuple v = (evald $ x v, evald $ y v)

> rotateTuple a (x,y) = ( x * c - y * s,
>                         x * s + y * c )
>     where c = cos a
>           s = sin a

> plusTuple (x1, y1) (x2, y2) = (x1+x2, y1+y2)

All values are in micro meters

> shaftVectors = map tupleToVector [(1,0),(1,0),(0,0),(1,0),(1,0)]

> boreVectors = map tupleToVector  [(0,1),(0,1),(1,1),(0,1),(0,1)]

> rotatedShaftVectors a = addVectorLists
>                         (map (rotateVector a) shaftVectors)
>                         boreVectors

> rotatedShaftConsolePoint sd a = vectorToPoint $
>                                 vectorFromRunouts sd rshv (0.*mm)
>     where rshv = rotatedShaftVectors a

> vectorFromRunouts sd runoutsVector coord =
>     head $ vectorsFromRunouts sd runoutsVector [coord]

> vectorsFromRunouts sd runoutsVector coords =
>     map (\ c -> tupleToVector (getSpindleDeflection xsd c /. micro meter,
>                                getSpindleDeflection ysd c /. micro meter))
>         coords
>     where xsd = substRunouts (map x runoutsVector) sd
>           ysd = substRunouts (map y runoutsVector) sd

> toRadians a = a / 180 * pi

little conclusion to testRunouts:
  Bore displacements only give us additional reactions
  and displaces shaft axis. They are doesn't influence spindle runout.
  Remark: stated above correct only for cases when bearing rigidity is linear
          (at least I think so).

> testRunouts angles = do
>     putStr "solving spindle ... "
>     generalSD <- solveOptimizedSpindle testSpindleWithRunouts
>     putStrLn "ok"
>              
>     putStr "rotating shaft ... "
>     let points = map (rotatedShaftConsolePoint generalSD . toRadians) angles
>     putStrLn "ok"
>              
>     putStr "exporting drawing ... "
>     exportToACAD (Line NormalLine points) "c:/runouts.lsp"
>     putStrLn "ok"


Some results for optimum runouts configuration:

for 5 bearings, 2.5 runout, we get that optimum is BMW like:
    angles: 0, 120, 240, 60, 0   (BMW)
    vector at   0mm: (-0.096, 0.053) length = runout at   0mm = 0.109
    vector at 300mm: ( 0.060, 0.108) length = runout at 300mm = 0.124
  0.124 it's pretty good for 2.5 initial run-out

After settings eccentricity = 1/2 run-out = 1/2 * 2.5 = 1.25
we get the same results, only vectors twice shorten.

for 4 bearing (3rd commented out), 1.25 eccentricity, we get:
    angles: 0, 180, _, 60, 300   (Pacific)
    vector at   0mm: (-0.122, -0.109) length = 0.164, runout = 0.328
    vector at 300mm: ( 0.015,  0.003) length = 0.016
  0.328 it's not so bad for 2.5 initial run-out

for 3 bearings (3,4 commented out), 1.25 eccentricity, we get:
    angles: 0, 120, _, _, 30 (strange?)
    angles: 0, 132, _, _, 33 (with 1 degree step), 0.337, runout 0.647
    vector at   0mm: (-0.123, 0.002) length = 0.123
    vector at 300mm: ( 0.250, 0.410) length = 0.480, runout = 0.960
  0.960 it's ~2.5 times smaller than 2.5 initial run-out
  0.647 it's ~3.8 times smaller than 2.5 initial run-out

for 2 bearings we get what we should get:
    angles: 0, 0 (direct both in one side)

Heh, not so bad!
Using different mount angles (not only 0 or 180) for bearing inner rings
we can reduce run-outs several times even for 3 bearings spindle.

little conclusion for bearings with different run-outs:
The bearing with smallest run-out not necessarily must be first.
Optimal configuration may differ for different run-out sets
(and also for different resolution in optimum search, e.g. for 10 degree step
we determine optimum more precisely and it's usuallt not same as for 30 degree step,
but we have problem of precise mounting for precise angles).

About bearing reactions. Reaction is usually 1+/-0.25 eccentricity * radial rigidity,
where eccentricity is 1/2 radial runout.
But it can be larger or smaller depending on runout values and spindle configuration.
We can (very) roughly assume that maximum reaction is
maximum radial runout * radial rigidity.

> testSeparateInnerRingRotation = do
>     --putStr "solving spindle ... "
>     sd <- solveOptimizedSpindle (console <+> testSpindleWithRunouts)
>     --putStrLn "ok"
>
>     let runouts = [1.25, 1.25, 1.25, 1.25, 1.25] -- [1.25, 1.75, 0.75,  1.0, 1.5]
>         permutedRunouts = [front ++ rear |
>                            front <- filterUnique $ permutations (take 3 runouts),
>                            rear  <- filterUnique $ permutations (drop 3 runouts)]
>     flip mapM_ permutedRunouts $ \ runouts -> do
>         mapM_ (printf "%4.2f ; " . evald) runouts
>         o <- searchOptimum2 sd runouts
>         printLine o
>	  print $ totalBearingReactions
>	    $ substAnglesAndRunouts
>	        ["roa", "rob", "roc", "rod", "roe"]
>	        (optimumAngles o)
>	        runouts
>	        sd
>	  print $ map (\(b,l,r) -> evald $ radialRigidity b /. (newton /. micro meter))
>	              (getBearingReactions sd)
>	      
>     let b1 = findBearingByCode "B7015C.T.P4S"
>         b2 = findBearingByCode "B7012C.T.P4S"
>         spindle = console <+> testSpindleConstructor 1 0 b1 b2 [0,0..]
>     baseSsd <- solveOptimizedSpindle spindle
>     let baseS = substdL 0 baseSsd
>     print $ 1000 / deflectionAt (0.*mm) baseS
>     print $ 1000 / deflectionAt consoleLength baseS
>    
>     putStrLn "done."

> optimumAngles (angles, _, _, _) = angles

> printLine (angles, (x1, y1, runout1), (x2, y2, runout2), maxRunout) = do
>     mapM_ (printf "%5.0f ; " . evald) angles
>     -- mapM_ (printf "%6.3f ; ") [x1, y1, x2, y2]
>     mapM_ (printf "%5.3f ; ") [runout1, runout2]
>     printf "%5.3f\n" maxRunout

In search of optimum inner ring rotation angles combination
we iterate over range of inner ring rotation angles and
substitute run-out values by rotated vector coordinates.
To get console run-out we use two independent spindle deflections values
for X0Z and for Y0Z planes.
Also we use additional console to get runouts at two points: end of longest part
and end of spindle shaft. Maximum run-out is used afterwards.

> searchOptimum sd [roa, rob, roc, rod, roe] = do
>     let range = [0,30..330]
>         halfRange = takeWhile (<= 180) range
>
>     optimum <- newIORef (undefined, undefined, undefined, 10000.0)
>     --n <- newIORef (1::Integer)
>
>     do flip mapM_ (substAngleRange "roa" (roa,0) [0] (sd,sd))   $ \ (_, sdxsdy) -> do
>        flip mapM_ (substAngleRange "rob" (rob,0) halfRange sdxsdy) $ \ (bAngle, sdxsdy) -> do
>        flip mapM_ (substAngleRange "roc" (roc,0) range sdxsdy)  $ \ (cAngle, sdxsdy) -> do
>        flip mapM_ (substAngleRange "rod" (rod,0) range sdxsdy)  $ \ (dAngle, sdxsdy) -> do
>        flip mapM_ (substAngleRange "roe" (roe,0) range sdxsdy)  $ \ (eAngle, (sdx,sdy)) -> do
>         let x1 = evald $ getSpindleDeflection sdx (0.*mm) /. micro meter
>             y1 = evald $ getSpindleDeflection sdy (0.*mm) /. micro meter
>             x2 = evald $ getSpindleDeflection sdx consoleLength /. micro meter
>             y2 = evald $ getSpindleDeflection sdy consoleLength /. micro meter
>             runout1 = sqrt $ x1*x1 + y1*y1
>             runout2 = sqrt $ x2*x2 + y2*y2
>             maxRunout = max runout1 runout2
>             line = ([0, bAngle, cAngle, dAngle, eAngle],
>                     (x1, y1, runout1),
>                     (x2, y2, runout2),
>                     maxRunout)
>         (_, _, _, minRunout) <- readIORef optimum
>         if maxRunout < minRunout
>          then
>            do modifyIORef optimum (\ _ -> line)
>          else
>            do return ()
>         --curn <- readIORef n
>         --printf "%5d ; " curn
>         --modifyIORef n (+ 1)
>         --printLine line
>     o <- readIORef optimum
>     return o

> substRange var range sd = map (\ val -> (val, subst val)) range
>     where subst val = substituteSpindleDeflectionsParams [(var, val)] sd

> substAngleRange var tvec angleRange (sdx, sdy) =
>       map (\ angle -> (angle, substAngle var tvec angle (sdx, sdy))) angleRange

> substAngle var tvec angle (sdx, sdy) =
>       (substituteSpindleDeflectionsParams [(var, x rv)] sdx,
>        substituteSpindleDeflectionsParams [(var, y rv)] sdy)
>     where vec = tupleToVector tvec
>           rv = rotateVector (toRadians angle) vec

> substAnglesAndRunouts vars angles runouts sd =
>     foldl (\ (sdx, sdy) (var, angle, runout) ->
>             substAngle var (runout, 0) angle (sdx, sdy))
>       (sd, sd)
>       (zip3 vars angles runouts)

> totalBearingReactions (sdx, sdy) =
>     map (\ ((b1, l1, r1), (b2, l2, r2)) ->
>            sqrt $ evald $ (r1*.r1 +. r2*.r2) /. square newton)
>        $ zip (getBearingReactions sdx)
>              (getBearingReactions sdy)

In search optimum II we use the fact that in linear system console run-out can be
calculated separately for each bearing run-out and then summed together.
So we calculate console run-out vectors and then search the minimal total run-out
by rotation of calculated vectors, not by rotating run-outs and calculating console
run-out after.

TODO: direct search of optimum angles is long. some optimization algorithm must exists.

> searchOptimum2 sd [roa, rob, roc, rod, roe] = do
>     let range = [0,10..350] -- much better (2,3x smaller runouts) than 30 degree step
>         halfRange = takeWhile (<= 180) range
>         toVectors = map (\ x -> Vector { x = x, y = 0 })
>         makeVecs r = map vectorToEvaluatedTuple $
>                      vectorsFromRunouts sd (toVectors r)
>                                         [0.*mm, consoleLength]
>         vecsa = makeVecs [roa, 0, 0, 0, 0]
>         vecsb = makeVecs [0, rob, 0, 0, 0]
>         vecsc = makeVecs [0, 0, roc, 0, 0]
>         vecsd = makeVecs [0, 0, 0, rod, 0]
>         vecse = makeVecs [0, 0, 0, 0, roe]
>         rotateAndSum [baseVec1, baseVec2] [vec1, vec2] angle =
>             (angle,
>              [rotateTuple rangle vec1 `plusTuple` baseVec1,
>               rotateTuple rangle vec2 `plusTuple` baseVec2])
>           where rangle = toRadians angle
>         scan base vecs range =
>             flip mapM_ (map (rotateAndSum base vecs) range)
>
>     --putStrLn ""
>     --mapM_ print [vecsa, vecsb, vecsc, vecsd, vecse]
>           
>     optimum <- newIORef (undefined, undefined, undefined, 10000.0)
>
>     do scan vecsa vecsb halfRange $ \ (bAngle, sum) -> do
>        scan sum   vecsc range     $ \ (cAngle, sum) -> do
>        scan sum   vecsd range     $ \ (dAngle, sum) -> do
>        scan sum   vecse range     $ \ (eAngle, sum) -> do
>         let [(x1,y1), (x2,y2)] = sum
>             runout1 = sqrt $ x1*x1 + y1*y1
>             runout2 = sqrt $ x2*x2 + y2*y2
>             maxRunout = max runout1 runout2
>             line = (map (fromRational . toRational) [0, bAngle, cAngle, dAngle, eAngle],
>                     (x1, y1, runout1),
>                     (x2, y2, runout2),
>                     maxRunout)
>         (_, _, _, minRunout) <- readIORef optimum
>         if maxRunout < minRunout
>          then
>            do modifyIORef optimum (\ _ -> line)
>          else
>            do return ()
>     o <- readIORef optimum
>     return o



Remark about optimum search:

In general when minimizing of two vector sums (with vectors constrained by the same
rotation angles) we have many local minimums (at least in case of 3 and more angles).

For example, here first five lines from 5 bearing angles optimization (in ascending order
from optimum), the last colum is maximum run-out achieved:
0;120;240; 60;  0; -0.048 ;  0.026 ;  0.030 ;  0.054 ; 0.055 ; 0.062 ; 0.062
0;150;240;330; 90;  0.067 ;  0.118 ; -0.047 ; -0.068 ; 0.136 ; 0.083 ; 0.136
0;150;270;330; 90;  0.102 ;  0.109 ;  0.101 ; -0.108 ; 0.149 ; 0.148 ; 0.149
0;150;210; 90;330;  0.153 ; -0.049 ; -0.148 ;  0.026 ; 0.161 ; 0.150 ; 0.161
0;150;210;330; 90;  0.042 ;  0.144 ; -0.156 ;  0.041 ; 0.150 ; 0.161 ; 0.161

As you can see the angles combinations are very different (they are not stay near each other).
So we can't use gradient-like optimization methods for finding optimum. We will
only find local optimum. The thing that could be done is to get from search space
a some reasonable amount of start points and then perform gradient search.

For now we simply scan search space at fixed step to find some minimum value
from scanned points (and it's even not a local minium). Taking into account, that
mounting bearing with angle not divisible by 30 (or at least 5) can be complicated,
this approach is (temporarily) ok.

When we add shaft runout, 6th bearing(?) or outer rings(?) we'll need more optimal search
even if it doesn't give us global optimum.
(especially with non-linear rigidity, when we need to solve system at each point, the search
can become crucial).


Rigidity, Optimal positions, Reactions.

j_r * [0.84-0.84-1.16 //\  1.0-1.0 /\]
  j_r at flange  = 151.1 N/mum
  j_r at console = 46.2 N/mum  (console length = 300 mm)

     1.25 ; 1.25 ; 1.25 ; 1.25 ; 1.25 ; eccentricity
     193.6, 193.6, 267.3, 193.5, 193.5; j_r

  step = 30 deg (r_c = 0.085; r_f = 0.117; r_max = 0.117)
        0 ;   90 ;  210 ;   30 ;   30 ; position
    [241.7, 212.4, 367.6,  46.7,  22.4] R_i
     
  step = 10 deg (r_c = 0.042; r_f = 0.029; r_max = 0.042)
        0 ;  110 ;  230 ;   70 ;  350 ; position
    [245.7, 235.7, 338.9, 167.3, 145.6] R_i

  step =  5 deg (r_c = 0.031; r_f = 0.026; r_max = 0.031)
        0 ;  110 ;  225 ;    0 ;   75 ; position
    [247.7, 235.9, 337.0, 160.0, 138.4] R_i


j_r * [1.0-1.0-1.0 //\  1.0-1.0 /\]
TBT set with DB rigidity (WRONG!!!)
  j_r at flange  = 162.4 N/mum
  j_r at console = 48.7 N/mum

     1.25 ; 1.25 ; 1.25 ; 1.25 ; 1.25 ; eccentricity
     230.4, 230.4, 230.4, 193.5, 193.5; j_r

  step = 30 deg (r_c = 0.055; r_f = 0.062; r_max = 0.062)
        0 ;  120 ;  240 ;   60 ;    0 ; position
    [277.2, 282.5, 311.2, 133.7, 112.4] R_i

  step = 10 deg (r_c = 0.033; r_f = 0.022; r_max = 0.033)
        0 ;  130 ;  240 ;   70 ;  350 ; position
    [290.3, 282.8, 295.4, 167.6, 144.6] R_i    

  step =  5 deg (r_c = 0.026; r_f = 0.018; r_max = 0.026)
        0 ;  135 ;  245 ;   75 ;  345 ; position
    [289.8, 289.3, 286.9, 184.4, 158.2] R_i
     

> dynamicReaction mass omega radius = mass *. square omega *. radius

For our test spindle (18kg shaft, 7000rpm max, 1.25mum eccentricity) the dynamic reaction
is only 12N, while one front bearing radial rigidity is 230.4N/mum. So dynamic reaction
is pretty small in comparison with static reaction (I think that in real constructions
center of mass eccentricity is much bigger than geometrical shaft axis eccentricity).

*Main> eval $ dynamicReaction (18.*kilogram) ((2*pi*7000).*rpm) (1.25.*micro meter) /. newton
12.090265391334462
*Main> eval $ (radialRigidity $ findBearingByCode "B7015C.T.P4S") /. (newton /. micro meter)
230.4

little conclusion about dynamicReaction:
The minimizing of spindle shaft runout has nothing to do with minimizing of
bearings dynamic reaction. In high speed cutting (for example, for 30000rpm
the reaction is 222N) it can possibly help in minimising such reaction.
But we can diminish dynamic reaction simply by moving center of mass,
e.g. by drilling some metal off the shaft. And this method work for any RPM
and any axial displacement.
Although the minimizing of run-out decreases dynamic reaction, the real problem
with dynamic reaction is problems with unprecise shaft, not bearings.


Evaluation utilities.

> permutations [] = [[]]
> permutations (h:t) = concat $ map (insert h) (permutations t)
>     where insert x [] = [[x]]
>           insert x (h:t) = (x:h:t) : map (\ m -> h:m) (insert x t)

> filterUnique :: (Ord a) => [a] -> [a]
> filterUnique = Set.toList . Set.fromList

> optimizeSpindleDeflections =
>     substdL 0 . substpi -- it not only optimize it also makes dL = 0

> optimizedSpindleDeflections =
>     optimizeSpindleDeflections . spindleDeflections

> solveOptimizedSpindle spindle = withInterpreter $ \i -> do
>     solvedSD <- solveSpindleDeflections i (optimizedSpindleDeflections spindle)
>     return solvedSD

Additional console

> consoleLength = 300.*mm

> console = cylinder (100.*mm) consoleLength

Test spindle without forces and with runouts as symbols.
The runout symbol can be substituted after with micro meters using substRunouts

> testSpindleWithRunouts = testSpindleConstructor 0 0 fagB7015C fagB7012C runouts
>   where fagB7015C = (findBearingByCode "B7015C.T.P4S")
>                     { innerRingRadialRunout = 1 .* micro meter 
>                     }
>         fagB7012C = (findBearingByCode "B7012C.T.P4S")
>                     { innerRingRadialRunout = 1 .* micro meter 
>                     }

Runouts symbols

> runouts = [Symbol "roa",
>            Symbol "rob",
>            Symbol "roc",
>            Symbol "rod",
>            Symbol "roe"]

> substRunouts [a, b, c, d, e] =
>     substituteSpindleDeflectionsParams [("roa", a),
>                                         ("rob", b),
>                                         ("roc", c),
>                                         ("rod", d),
>                                         ("roe", e)]

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

> testSpindleConstructor f offset b1 b2 ro =
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
>     `modify` (if offset == 0 then (\s _ -> s) else addBendingMoment ((f*offset).*newton*.meter)) `at` 1.*mm
>     -- front bearing set
>     `modify` addBearing' b1_1st MountLeft  (ro!!0) `at` (44+27+0.5*wd1).*mm
>     `modify` addBearing' b1_2nd MountLeft  (ro!!1) `at` (44+47+1.5*wd1).*mm
>     `modify` addBearing' b1_3rd MountRight (ro!!2) `at` (44+79+2.5*wd1).*mm
>     -- rear bearing set
>     `modify` addBearing' b2_1st MountLeft  (ro!!3) `at` (281.5+31+3*wd1+0.5*wd2).*mm
>     `modify` addBearing' b2_2nd MountRight (ro!!4) `at` (281.5+49+3*wd1+1.5*wd2).*mm)
>     -- section where dL is added to length
>     `modify` (\ s _ -> s { sectionLength = sectionLength s
>                                   +. Symbol "dL" .* meter 
>                                 })
>              `at` ((44+100+3*wd1+1).*mm) 
>     -- end
>   where cyl d l = cylinder (d.*mm) (l.*mm)
>         wd1 = (width b1 -. width fagB7015C) /. mm
>         wd2 = (width b2 -. width fagB7012C) /. mm
>	  scaleRR s b = b { radialRigidity = s .* radialRigidity b }
>         b1_1st = scaleRR 0.84 b1
>         b1_2nd = scaleRR 0.84 b1
>         b1_3rd = scaleRR 1.16 b1
>         b2_1st = b2
>         b2_2nd = b2
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
