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
import ElementMatrix
import Material
import CrossSection
import Node
import Element
import Elements.TwoNodeBar2D
import Elements.TimoshenkoBeam2D
 
-- | Main program
main :: IO ()
main = print $ timoshenkoBeam2D n1 n2 steel (circle 100)
  where n1 = xyc (0,1,2) (0  ,0,0)
        n2 = xyc (3,4,5) (100,0,0)
