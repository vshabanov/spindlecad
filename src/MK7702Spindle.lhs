--
--  Copyright (C) 2008 Vladimir Shabanov
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

> drawingsPath = "./"

> drawPicture fileName picture = 
>     exportToACAD picture (drawingsPath ++ fileName ++ ".lsp")

> drawSpindle' mapDrawing dL fileName spindle sd = 
>     drawPicture fileName $ mapDrawing $
>                     (deflectionLine (10 .* mm /. nano meter) sd
>                      `Over`
>                      substituteDrawing (Map.fromList [("dL",dL)])
>                      (spindleDrawing spindle))

> drawSpindle mapDrawing dL fileName spindle = withInterpreter $ \i -> do
>     sd <- solveOptimizedSpindle spindle
>     drawSpindle' mapDrawing dL fileName spindle sd

> drawMK7702Spindle = drawSpindle id 0 "MK7702Spindle" mk7702Spindle

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
>         --          (bearingReactions s)
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
>     putStr "solving spindle ... "
>     sd <- solveOptimizedSpindle (console <+> testSpindleWithRunouts)
>     putStrLn "ok"
>
>     let runouts = [1.25, 1.25, 1.25, 2.5, 0.0] -- [1.25, 1.75, 0.75,  1.0, 1.5]
>         permutedRunouts = [front ++ rear |
>                            front <- filterUnique $ permutations (take 3 runouts),
>                            rear  <- {-filterUnique $ permutations-} [drop 3 runouts]]
>     flip mapM_ permutedRunouts $ \ runouts -> do
>         mapM_ (printf "%4.2f ; " . evald) runouts
>         o <- searchOptimum2 sd runouts
>         printLine o
>	  printTotalBearingReactions
>	    $ substAnglesAndRunouts
>	        ["roa", "rob", "roc", "rod", "roe"]
>	        (optimumAngles o)
>	        runouts
>	        sd
>     printBearingRigidities sd

> optimumAngles (angles, _, _, _) = angles

> printLine (angles, (x1, y1, runout1), (x2, y2, runout2), maxRunout) = do
>     mapM_ (printf "%5.0f ; " . evald) angles
>     -- mapM_ (printf "%6.3f ; ") [x1, y1, x2, y2]
>     mapM_ (printf "%5.3f ; ") [runout1, runout2]
>     printf "%5.3f\n" maxRunout

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

In search optimum II we use the fact that in linear system console run-out can be
calculated separately for each bearing run-out and then summed together.
So we calculate console run-out vectors and then search the minimal total run-out
by rotation of calculated vectors, not by rotating run-outs and calculating console
run-out after.

TODO: direct search of optimum angles is long. some optimization algorithm must exists.

> searchOptimum2 sd [roa, rob, roc, rod, roe] = do
>     let range = [0,30..330] -- much better (2,3x smaller runouts) than 30 degree step
>     --let range = [0,10..350] -- much better (2,3x smaller runouts) than 30 degree step
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

j_r * [0.84/  0.84/  1.16\   1.0/  1.0\]

  j_flange  = 151.1 N/mum
  j_console = 46.2 N/mum  (console length 300 mm, diameter 100 mm)

  eccentricity:  1.25       j_r :  193.6  193.6  267.3  193.5  193.5     

   step   a1   a2   a3   a4   a5    R1     R2     R3     R4     R5     r_c   r_f  r_max
  -------------------------------------------------------------------------------------
  30 deg   0   90  210   30   30   241.7  212.4  367.6   46.7   22.4  0.085 0.117 0.117
  10 deg   0  110  230   70  350   245.7  235.7  338.9  167.3  145.6  0.042 0.029 0.042
   5 deg   0  110  225    0   75   247.7  235.9  337.0  160.0  138.4  0.031 0.026 0.031 

j_r * [1.0/  1.0/  1.0\   1.0/  1.0\]  (TBT set with DB rigidity -- WRONG!!!)

  j_flange  = 162.4 N/mum
  j_console = 48.7 N/mum  (console length = 300 mm, diameter 100 mm)

  eccentricity:  1.25       j_r :  230.4  230.4  230.4  193.5  193.5

   step   a1   a2   a3   a4   a5    R1     R2     R3     R4     R5     r_c   r_f  r_max
  -------------------------------------------------------------------------------------
  30 deg   0  120  240   60    0   277.2  282.5  311.2  133.7  112.4  0.055 0.062 0.062
  10 deg   0  130  240   70  350   290.3  282.8  295.4  167.6  144.6  0.033 0.022 0.033
   5 deg   0  135  245   75  345   289.8  289.3  286.9  184.4  158.2  0.026 0.018 0.026
     

Evaluation utilities.

> permutations [] = [[]]
> permutations (h:t) = concat $ map (insert h) (permutations t)
>     where insert x [] = [[x]]
>           insert x (h:t) = (x:h:t) : map (\ m -> h:m) (insert x t)

> filterUnique :: (Ord a) => [a] -> [a]
> filterUnique = Set.toList . Set.fromList

> optimizeSpindleDeflections :: SpindleDeflections -> SpindleDeflections
> optimizeSpindleDeflections =
>     substdL 0 . substpi -- it not only optimize it also makes dL = 0

> optimizedSpindleDeflections :: Spindle -> SpindleDeflections
> optimizedSpindleDeflections =
>     optimizeSpindleDeflections . spindleDeflections

> solveOptimizedSpindle :: Spindle -> IO SpindleDeflections
> solveOptimizedSpindle spindle = withInterpreter $ \i ->
>     solveSpindleDeflections i (optimizedSpindleDeflections spindle)


Additional console

> consoleLength = 400.*mm

> console = cylinder (100.*mm) consoleLength

Test spindle without forces and with runouts as symbols.
The runout symbol can be substituted after with micro meters using substRunouts

> testSpindleWithRunouts = testSpindleConstructor 0 0 b1 b2 runouts
>     where b1 = frontBearing
>                { innerRingRadialRunout = 1 .* micro meter 
>                }
>           b2 = rearBearing
>                { innerRingRadialRunout = 1 .* micro meter 
>                }

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

> substdL dl = substituteSpindleDeflectionsParams [("dL", dl)]

> substpi = substituteSpindleDeflectionsParams [("_cas_pi", Rational $ toRational pi)]

> evald a = eval a :: Double

> relativeLives baseLife s =
>     map (\ (b, pos, r) -> (((evald $ cdyn b /. newton)
>                             /
>                             (abs $ evald $ r /. newton))**3 * 10^6 / (60*7000))
>                            / baseLife)
>             (bearingReactions s)

> relativeLife baseLife s = minimum $ relativeLives baseLife s

Console deflection, nano meter per 1 newton

> deflection s = (abs $ evald (getSpindleDeflection s (0.*mm) /. nano meter))

> deflectionAt pos s = (abs $ evald (getSpindleDeflection s pos /. nano meter))

Spindle rigidity N/mum
Remark that ridity is only valid when force applied at 0.*mm

> rigidity s = (abs $ evald ((1 ./ getSpindleDeflection s (0.*mm)) *. micro meter))

> printRigidity spindleDeflections = do
>     putStrLn $ "Rigidity     : " ++ show (rigidity spindleDeflections) ++ " N/mum"

Deflection line drawing

> deflectionLine scale s = Line NormalLine $
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

Bearing reactions and rigidities printers

> printBearingRigidities' :: String -> [BearingReaction] -> IO ()
> printBearingRigidities' prefix reactions = do
>     putStr prefix
>     let rigidity (b,_,_) = radialRigidity b /. (newton /. micro meter)
>     mapM_ (printf "%6.1f  " . evald . rigidity) reactions
>     putStrLn ""

> printBearingRigidities :: SpindleDeflections -> IO ()
> printBearingRigidities spindleDeflections =
>     printBearingRigidities' "Rigidities   : " (bearingReactions spindleDeflections)

> printBearingReactions' :: String -> [BearingReaction] -> IO ()
> printBearingReactions' prefix reactions = do
>     putStr prefix
>     let reaction (_,_,r) = r /. newton
>     mapM_ (printf "%6.3f  " . evald . reaction) reactions
>     putStrLn ""

> printBearingReactions :: SpindleDeflections -> IO ()
> printBearingReactions spindleDeflections =
>     printBearingReactions'  "Reactions    : " (bearingReactions spindleDeflections)

> printTotalBearingReactions :: (SpindleDeflections, SpindleDeflections) -> IO ()
> printTotalBearingReactions (sdy, sdz) = do
>     printBearingReactions'  "Reactions Y  : " (bearingReactions sdy)
>     printBearingReactions'  "Reactions Z  : " (bearingReactions sdz)
>     printBearingReactions'  "Reactions Sum: " (totalBearingReactions (sdy, sdz))

Test spindle rigidity in two cases:
 1. flange force
 2. console force (console assumed to be absolutely rigid,
    so we just apply force at flange plus bending moment

> testSpindleOptimalLength =
>     testSpindleOptimalLength' spindleConstructorMK7702

> testSpindleOptimalLength' spindleConstructor = do
>     let sc f c = spindleConstructor f c frontBearing rearBearing [0,0..]
>         flangeSpindle  = sc 1 0
>         consoleSpindle consoleLength =
>             (show (round $ evald $ consoleLength /. mm),
>              move (0.*mm -. consoleLength) (0.*mm), -- move spindle drawing by consoleLength
>              cylinder (100.*mm) consoleLength <+> sc 1 (consoleLength/.meter))
>         consoleSpindles = map consoleSpindle $ map (.*mm) [100,200,300,400]
>     findOptimalLength id "MK7702Flange"  "FORCE ON FLANGE"        flangeSpindle
>     flip mapM_ consoleSpindles $ \ (suffix, mapDrawing, spindle) ->
>         findOptimalLength mapDrawing
>                               ("MK7702Console" ++ suffix)
>                               ("FORCE ON RIGID CONSOLE OF " ++ suffix ++ " mm")
>                               spindle


> findOptimalLength mapDrawing spindleName experimentName spindle = do
>     putStrLn experimentName
>     sd <- withInterpreter (\ i -> solveSpindleDeflections i $ substpi $ spindleDeflections spindle)
>     --sd <- withInterpreter (\ i -> solveOptimizedSpindle spindle)
>     let sd0 = getSpindleDeflection sd (0.*mm)
>         sd0dlList = map (\ dl -> (substitutepv' [("dL", dl)] sd0 /. nano meter,
>                                   dl)) $ [-200..500]
>         lengthPlot = Line NormalLine $
>                      map (\ (sd, dl) -> Point ((dl + 348.5 + 77 + 14).*mm) ((10*sd).*mm)) sd0dlList
>         (sd0opt, dLopt) = minimum $ map (\(sd,dl) -> (abs $ evald $ sd, dl)) sd0dlList
>         s = substdL 0 sd
>         sopt = substdL dLopt sd
>     drawPicture (spindleName ++ "LengthOptimizationPlot") lengthPlot
>     drawSpindle' mapDrawing 0      spindleName                       spindle s
>     drawSpindle' mapDrawing dLopt (spindleName ++ "OptimizedLength") spindle sopt
>                
>     printBearingRigidities s
>                            
>     putStrLn "\nBase spindle..."
>     printRigidity s
>     printBearingReactions s
>                           
>     putStrLn "\nSpindle with optimized length..."
>     printf "dL = %5.1f; Length = %5.1f\n" (evald dLopt) (evald dLopt + 348.5)
>     printRigidity sopt
>     printBearingReactions sopt
>                           
>     putStrLn "\n"


Spindle description construction.

> fagB71922E = findBearingByCode "B71922E.T.P4S"
> fagNN3020  = findBearingByCode "NN3020ASK.M.SP"
> frontBearing = fagB71922E
> rearBearing  = fagNN3020

> mk7702Spindle = spindleConstructorMK7702 1 0 frontBearing rearBearing [0,0,0,0]

> testSpindleConstructor = spindleConstructorMK7702
> testSpindle = mk7702Spindle

> spindleConstructorMK7702 consoleForce forceOffset b1 b2 ro =
>     -- shaft
>     ((cyl 106 14 <+> cyl 170 29 <+> cyl 150 9
>       <+> cyl 117 19
>       <+> cyl (innerDiameter b1 /. mm) (3 * w1 + spacer + gaika1)
>       <+> cyl 107.5
>           (348.5
>            - (2 * w1 + spacer + gaika1 + vtulka2 + (width b2 /. mm)/2))
>       <+> cyl (w2 / 12     + innerDiameter b2 /. mm) vtulka2
>       <+> cyl (w2 / 12 / 2 + innerDiameter b2 /. mm) w2
>       <+> cyl (innerDiameter b2 /. mm) 79
>       <+> cyl 95 6
>       <+> cyl 97 (109+35)
>       <+> cyl 92 8
>       <+> cyl 97 16)
>      `cut`
>      (cyl 81 109 <+> cyl 74 (711 - 109))
>      --(cyl 76 109 <+> cyl 69 (711 - 109))
>      --(cyl 71 109 <+> cyl 64 (711 - 109))
>      )
>     -- forces
>     `modifyIf` (consoleForce /= 0, addRadialForce (consoleForce.*newton) `at` 0.*mm)
>     `modifyIf` (forceOffset  /= 0, addBendingMoment ((consoleForce*forceOffset).*newton*.meter) `at` 1.*mm)
>     -- front bearing set
>     `modify` addBearing' b1_1st MountLeft  (ro!!0) `at` (14+77-0.5*w1).*mm
>     `modify` addBearing' b1_2nd MountLeft  (ro!!1) `at` (14+77+0.5*w1).*mm
>     `modify` addBearing' b1_3rd MountRight (ro!!2) `at` (14+77+spacer+1.5*w1).*mm
>     --`modify` addBearing' (realScaleRR (1.36*2) b1) MountRight (ro!!2) `at` (14+77+0.5*w1).*mm
>     --`modify` addBearing' b1_3rd MountRight (ro!!2) `at` (14+77+{-spacer+-}1.5*w1).*mm
>     -- rear bearing 
>     `modify` addBearing' b2     MountLeft  (ro!!3) `at` (14+77+348.5).*mm
>     --`modify` addBearing' b2_2     MountLeft  (ro!!3) `at` (14+77+348.5-9).*mm
>     --`modify` addBearing' b2_2     MountLeft  (ro!!3) `at` (14+77+348.5+9).*mm
>     --`modify` addBearing' b2_4     MountLeft  (ro!!3) `at` (14+77+348.5-9-11/2).*mm
>     --`modify` addBearing' b2_4     MountLeft  (ro!!3) `at` (14+77+348.5-9+11/2).*mm
>     --`modify` addBearing' b2_4     MountLeft  (ro!!3) `at` (14+77+348.5+9-11/2).*mm
>     --`modify` addBearing' b2_4     MountLeft  (ro!!3) `at` (14+77+348.5+9+11/2).*mm
>     -- section where dL is added to length
>     `modify` (\ s _ -> s { sectionLength = sectionLength s
>                                   +. Symbol "dL" .* mm 
>                                 })
>              `at` (250.*mm)
>     -- end
>   where cyl d l = cylinder (d.*mm) (l.*mm)
>         spacer = 10
>         gaika1 = 43 -- gaika+vtulka
>         vtulka2 = 15
>         w1 = width b1 /. mm
>         w2 = width b2 /. mm
>         -- width difference compared to initial spindle
>         --wd1 = (width b1 -. width fagB7015C) /. mm
>         --wd2 = (width b2 -. width fagB7012C) /. mm
>         -- we can scale radial rigidity to account that bearings are
>         -- loaded differently in TBT set //\ - 0.84/0.84/1.16\
>         realScaleRR s b = b { radialRigidity = s .* radialRigidity b }
>         scaleRR s b = b -- no scale
>         --b1_1st = scaleRR 0.84 b1
>         --b1_2nd = scaleRR 0.84 b1
>         --b1_3rd = scaleRR 1.16 b1
>         b1_scaled = {-realScaleRR 1.1-} b1
>         b1_1st = b1_scaled
>         b1_2nd = b1_scaled
>         b1_3rd = b1_scaled
>         b2_2 = realScaleRR 0.5 b2
>         b2_4 = realScaleRR 0.25 b2


1-point roller bearing, no spacer

FORCE ON FLANGE
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 267.73109401306334 N/mum
Reactions    : -0.502  -0.428  -0.360   0.290  
Reactions    : -0.517  -0.441  -0.340   0.298  -- spacer case

Spindle with optimized length...
dL =  24.0; Length = 372.5
Rigidity     : 268.1568860439052 N/mum
Reactions    : -0.497  -0.421  -0.353   0.271  


FORCE ON RIGID CONSOLE OF 100 mm
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 107.37212721639466 N/mum
Reactions    : -0.652  -0.521  -0.406   0.579  

Spindle with optimized length...
dL = -38.0; Length = 310.5
Rigidity     : 108.07621731830012 N/mum
Reactions    : -0.673  -0.546  -0.435   0.653  


FORCE ON RIGID CONSOLE OF 200 mm
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 54.36298148827296 N/mum
Reactions    : -0.801  -0.615  -0.452   0.869  

Spindle with optimized length...
dL = -60.0; Length = 288.5
Rigidity     : 55.42535418498052 N/mum
Reactions    : -0.855  -0.678  -0.524   1.057  


FORCE ON RIGID CONSOLE OF 300 mm
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 32.279420758399425 N/mum
Reactions    : -0.951  -0.709  -0.498   1.158  

Spindle with optimized length...
dL = -71.0; Length = 277.5
Rigidity     : 33.245168497803306 N/mum
Reactions    : -1.039  -0.812  -0.617   1.468  


FORCE ON RIGID CONSOLE OF 400 mm
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 21.246189534623394 N/mum
Reactions    : -1.100  -0.803  -0.544   1.447  

Spindle with optimized length...
dL = -78.0; Length = 270.5
Rigidity     : 22.04598492804108 N/mum
Reactions    : -1.225  -0.948  -0.711   1.884  


1-point roller bearing

FORCE ON FLANGE
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 260.9192641748797 N/mum
Reactions    : -0.517  -0.441  -0.340   0.298  
Reactions    : -0.517  -0.441  -0.339   0.140   0.157  
Reactions    : -0.517  -0.441  -0.339   0.068   0.073   0.076   0.081  

               
               
Rigidities   :  289.5   289.5   289.5  2170.0  
Rigidity     : 305.44510330083125 N/mum -- B71922C.T.P4S instead of B71922C.T.P4S
                                        -- (15deg instead of 25deg)
                                        --             k  +17%
                                        -- but axial rigidy 371.1=>158.2 (2.35 times lower)

Rigidities   :  248.9   248.9   248.9  2170.0  
Rigidity     : 277.74742051477926 N/mum -- front bearing j +10%,
                                        --             k  +6.4%
Reactions    : -0.522  -0.441  -0.334   0.297  

Rigidities   :  226.3   226.3   226.3  2170.0  
Reactions    : -0.531  -0.441  -0.323   0.296  
Rigidity     : 267.73109401306334 N/mum -- w/o spacer, k  +2.6%
Reactions    : -0.502  -0.428  -0.360   0.290  
Rigidity     : 253.03161601242942 N/mum -- hole  +5mm, k  -3.1%
Reactions    : -0.522  -0.441  -0.335   0.297  
Rigidity     : 266.71792502748013 N/mum -- hole  -5mm, k  +2.2%
Reactions    : -0.514  -0.441  -0.343   0.298  
Rigidity     : 271.02861649449443 N/mum -- hole -10mm, k  +3.8%
Reactions    : -0.512  -0.441  -0.346   0.299  

Rigidities   :  615.5  2170.0  
Rigidity     : 245.92943123161749 N/mum -- 1.36*Sr single second bearing -6%, -8.8% (w/o spacer)
Reactions    : -1.298   0.298  
Rigidity     : 305.1272596332921 N/mum  -- -/- single first bearing +17% (big error)
Reactions    : -1.226   0.226  

Spindle with optimized length...
dL =  25.0; Length = 373.5
Rigidity     : 261.3706405057058 N/mum
Reactions    : -0.512  -0.434  -0.331   0.277  


FORCE ON RIGID CONSOLE OF 100 mm
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 105.32109304165355 N/mum
Reactions    : -0.674  -0.542  -0.371   0.587  

Spindle with optimized length...
dL = -35.0; Length = 313.5
Rigidity     : 105.92451406063618 N/mum
Reactions    : -0.693  -0.564  -0.399   0.656  


FORCE ON RIGID CONSOLE OF 200 mm
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 53.54847866884522 N/mum
Reactions    : -0.831  -0.642  -0.402   0.876  

Spindle with optimized length...
dL = -57.0; Length = 291.5
Rigidity     : 54.496588059311804 N/mum
Reactions    : -0.881  -0.701  -0.475   1.057  


FORCE ON RIGID CONSOLE OF 300 mm
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 31.875272247044837 N/mum
Reactions    : -0.988  -0.743  -0.434   1.165  

Spindle with optimized length...
dL = -68.0; Length = 280.5
Rigidity     : 32.748664346868175 N/mum
Reactions    : -1.071  -0.841  -0.553   1.465  


FORCE ON RIGID CONSOLE OF 400 mm
Rigidities   :  226.3   226.3   226.3  2170.0  

Base spindle...
Rigidity     : 21.01387012487501 N/mum
Reactions    : -1.145  -0.844  -0.465   1.454  

Spindle with optimized length...
dL = -75.0; Length = 273.5
Rigidity     : 21.742281374411252 N/mum
Reactions    : -1.263  -0.982  -0.633   1.878  


2-points roller bearing

FORCE ON FLANGE
Rigidities   :  226.3   226.3   226.3  1085.0  1085.0  

Base spindle...
Rigidity     : 260.92639047888855 N/mum
Reactions    : -0.517  -0.441  -0.339   0.140   0.157  

Spindle with optimized length...
dL =  25.0; Length = 373.5
Rigidity     : 261.36904423611225 N/mum
Reactions    : -0.512  -0.434  -0.331   0.139   0.138  


FORCE ON RIGID CONSOLE OF 100 mm
Rigidities   :  226.3   226.3   226.3  1085.0  1085.0  

Base spindle...
Rigidity     : 105.3322489545351 N/mum
Reactions    : -0.675  -0.542  -0.372   0.318   0.270  

Spindle with optimized length...
dL = -35.0; Length = 313.5
Rigidity     : 105.92305636608619 N/mum
Reactions    : -0.693  -0.564  -0.399   0.329   0.327  


FORCE ON RIGID CONSOLE OF 200 mm
Rigidities   :  226.3   226.3   226.3  1085.0  1085.0  

Base spindle...
Rigidity     : 53.56533321725199 N/mum
Reactions    : -0.832  -0.643  -0.404   0.496   0.382  

Spindle with optimized length...
dL = -57.0; Length = 291.5
Rigidity     : 54.4955821224433 N/mum
Reactions    : -0.881  -0.701  -0.475   0.531   0.527  


FORCE ON RIGID CONSOLE OF 300 mm
Rigidities   :  226.3   226.3   226.3  1085.0  1085.0  

Base spindle...
Rigidity     : 31.890231236381748 N/mum
Reactions    : -0.989  -0.745  -0.436   0.674   0.495  

Spindle with optimized length...
dL = -68.0; Length = 280.5
Rigidity     : 32.74797116600126 N/mum
Reactions    : -1.071  -0.841  -0.553   0.735   0.729  


FORCE ON RIGID CONSOLE OF 400 mm
Rigidities   :  226.3   226.3   226.3  1085.0  1085.0  

Base spindle...
Rigidity     : 21.026031954395105 N/mum
Reactions    : -1.146  -0.846  -0.468   0.852   0.608  

Spindle with optimized length...
dL = -75.0; Length = 273.5
Rigidity     : 21.74177697485574 N/mum
Reactions    : -1.263  -0.982  -0.633   0.942   0.935  


4-points roller bearing

FORCE ON FLANGE
Rigidities   :  226.3   226.3   226.3   542.5   542.5   542.5   542.5  

Base spindle...
Rigidity     : 260.9284284452402 N/mum
Reactions    : -0.517  -0.441  -0.339   0.068   0.073   0.076   0.081  

Spindle with optimized length...
dL =  25.0; Length = 373.5
Rigidity     : 261.36796046015564 N/mum
Reactions    : -0.512  -0.434  -0.331   0.070   0.069   0.069   0.069  


FORCE ON RIGID CONSOLE OF 100 mm
Rigidities   :  226.3   226.3   226.3   542.5   542.5   542.5   542.5  

Base spindle...
Rigidity     : 105.33590601965817 N/mum
Reactions    : -0.675  -0.542  -0.372   0.166   0.152   0.143   0.128  

Spindle with optimized length...
dL = -35.0; Length = 313.5
Rigidity     : 105.92206250182281 N/mum
Reactions    : -0.693  -0.564  -0.399   0.165   0.164   0.164   0.163  


FORCE ON RIGID CONSOLE OF 200 mm
Rigidities   :  226.3   226.3   226.3   542.5   542.5   542.5   542.5  

Base spindle...
Rigidity     : 53.57121942708758 N/mum
Reactions    : -0.832  -0.644  -0.404   0.265   0.231   0.209   0.174  

Spindle with optimized length...
dL = -57.0; Length = 291.5
Rigidity     : 54.49489755788667 N/mum
Reactions    : -0.881  -0.701  -0.475   0.266   0.265   0.264   0.263  


FORCE ON RIGID CONSOLE OF 300 mm
Rigidities   :  226.3   226.3   226.3   542.5   542.5   542.5   542.5  

Base spindle...
Rigidity     : 31.895515248122663 N/mum
Reactions    : -0.990  -0.745  -0.436   0.364   0.310   0.276   0.221  

Spindle with optimized length...
dL = -68.0; Length = 280.5
Rigidity     : 32.747498107026615 N/mum
Reactions    : -1.071  -0.841  -0.553   0.368   0.367   0.366   0.364  


FORCE ON RIGID CONSOLE OF 400 mm
Rigidities   :  226.3   226.3   226.3   542.5   542.5   542.5   542.5  

Base spindle...
Rigidity     : 21.03034790096615 N/mum
Reactions    : -1.147  -0.847  -0.469   0.463   0.390   0.342   0.268  

Spindle with optimized length...
dL = -75.0; Length = 273.5
Rigidity     : 21.741433519685458 N/mum
Reactions    : -1.263  -0.982  -0.633   0.472   0.470   0.469   0.466  


Seems to be no major difference for calculating rigidity
