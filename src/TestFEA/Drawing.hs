-- | 
-- Module      :  TestFEA.Drawing
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Various drawing utilities
--
module Drawing (
    centerLine, thickLine, thinLine, lightLine, ultraLightLine
    ) where

import Graphics.Rendering.Cairo

-- | Used as center line of symmetrical figures
centerLine :: Render ()
centerLine = do
    setSourceRGB 0.5 0.5 0.5
    setLineWidth 1
    setDash [25, 4, 8, 4] 0
    setLineCap LineCapRound

-- | Used as default line for figure boundaries
thickLine :: Render ()
thickLine = do
    setSourceRGB 0.3 0.3 0.3
    setLineWidth 2
    setDash [] 0
    setLineCap LineCapRound

-- | Line for figure boundaries inside figure
thinLine :: Render ()
thinLine = do
    setSourceRGB 0.3 0.3 0.3
    setLineWidth 1
    setDash [] 0
    setLineCap LineCapRound

-- | Virtual lines (e.g. deformed cross sections)
lightLine :: Render ()
lightLine = do
    setSourceRGB 0.7 0.7 0.7
    setLineWidth 1
    setDash [] 0
    setLineCap LineCapRound

-- | Thin virtual lines (displacements)
ultraLightLine :: Render ()
ultraLightLine = do
    setSourceRGB 0.9 0.9 0.9
    setLineWidth 1
    setDash [] 0
    setLineCap LineCapRound
