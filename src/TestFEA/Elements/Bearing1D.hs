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
import Element
import ElementMatrix
import Material
import CrossSection

-- | Y only bearing @linearYBearing node rigidity[N/mm]@.
linearYBearing :: Node.XYC -> D -> E
linearYBearing node rigidity =
    linearElement
    (matrix 3 $ map (rigidity*) [ 0, 0, 0,
                                  0, 1, 0,
                                  0, 0, 0 ])
    (fi [i1,i2,i3])
    (\ x -> return ())
    where (i1, i2, i3) = xycFI node
