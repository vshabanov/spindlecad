-- | 
-- Module      :  TestFEA.Elements.Bearing1D
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- 1D linear bearing
--
module Elements.Bearing1D (
    linearYBearing,
    ) where

import Node
import qualified Element
import ElementMatrix

-- | Y only bearing @linearYBearing node rigidity[N/mm]@.
linearYBearing :: Node.XYC -> D -> Element.E
linearYBearing node rigidity =
    Element.linear
    (matrix 1 [rigidity])
    (freedomIndices [i2])
    Element.noRender
    where (_i1, i2, _i3) = xycFI node
