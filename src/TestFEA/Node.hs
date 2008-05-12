-- | 
-- Module      :  TestFEA.Nodes
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Finite element nodes description.
--
module Node (
    -- * Types
    XY,
    XYZAB,
    -- * Creation
    xy, xyzab,
    -- * Queries
    xyFI, xyCoords
    ) where

import ElementMatrix (I, D, FI)

-- | 2D node containing two indices in assembled matrix
data XY = XY (I, I) (D, D)

-- | XY node description
xy :: (I, I) -> (D, D) -> XY
xy = XY

-- | Freedom indices
xyFI :: XY -> (I, I)
xyFI (XY (i1, i2) _) = (i1, i2)

-- | Coordinates
xyCoords :: XY -> (D, D)
xyCoords (XY _ (x, y)) = (x, y)

-- | 5D node containing five indices in assembled matrix
data XYZAB = XYZAB (I, I, I, I, I) (D, D, D, D, D)

-- | XYZAB node description
xyzab :: (I, I, I, I, I) -> (D, D, D, D, D) -> XYZAB
xyzab = XYZAB
