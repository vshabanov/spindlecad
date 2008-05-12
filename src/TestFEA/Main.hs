-- | 
-- Module      :  TestFEA.Main
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Stub 'do-nothing' main module
module Main (
    main
    ) where

import System.Environment
import Control.Monad
import ElementMatrix
import Material
import CrossSection
import Node
import Element
import Elements
 
-- | Main program
main :: IO ()
main = do
    --print $ elements
    --disp masterStiffness
    --print displacements
    print $ getv displacements 1
    print $ 1 / (getv displacements 1 * 1000)
  where n1 = xyc (1 , 2, 3) (0  ,0,0)
        n2 = xyc (4 , 5, 6) (100,0,0)
        n3 = xyc (7 , 8, 9) (124,0,0)
        n4 = xyc (10,11,12) (148,0,0)
        n5 = xyc (13,14,15) (900,0,0)
        cs = ring 0 100
        rigidity = 120000 -- N/mm
        elements =
            [
             bernoulliEulerBeam2D n1 n2 steel cs,
             bernoulliEulerBeam2D n2 n3 steel cs,
             bernoulliEulerBeam2D n3 n4 steel cs,
             bernoulliEulerBeam2D n4 n5 steel cs,
--              timoshenkoBeam2D n1 n2 steel cs,
--              timoshenkoBeam2D n2 n3 steel cs,
--              timoshenkoBeam2D n3 n4 steel cs,
--              timoshenkoBeam2D n4 n5 steel cs,
             linearYBearing n2 rigidity,
             linearYBearing n3 rigidity,
             linearYBearing n4 rigidity,
             linearYBearing n5 rigidity
            ]
        masterStiffness = assemble $ zip
                          (map stiffnessMatrix elements)
                          (map freedomIndices elements)
        masterForces = vector 15 ([0, 1] ++ replicate (15-2) 0)
        displacements = solve masterStiffness masterForces
