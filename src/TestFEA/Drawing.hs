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
    centerLine, thickLine, thinLine
    ) where

import Graphics.Rendering.Cairo

centerLine :: Render ()
centerLine = do
    setSourceRGB 0.5 0.5 0.5
    setLineWidth 1
    setDash [25, 4, 8, 4] 0
    setLineCap LineCapRound

thickLine :: Render ()
thickLine = do
    setSourceRGB 0.3 0.3 0.3
    setLineWidth 2
    setDash [] 0
    setLineCap LineCapRound

thinLine :: Render ()
thinLine = do
    setSourceRGB 0.7 0.7 0.7
    setLineWidth 1
    setDash [] 0
    setLineCap LineCapRound
