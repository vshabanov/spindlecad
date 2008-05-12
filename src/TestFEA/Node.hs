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
    I2, I3, I4, I5,
    C,
    C2, C3, C4, C5,
    XY,    
--    YC,
    XYC,
    XYZAB,
    -- * Creation
    xy, xyc, xyzab,
    -- * Queries
    xyFI, xyCoords,
    xycFI, xycCoords,
    xyzabFI, xyzabCoords,
    ) where

import ElementMatrix (I, D, FI)

-- | Two indices type
type I2 = (I, I)
-- | Three indices type
type I3 = (I, I, I)
-- | Four indices type
type I4 = (I, I, I, I)
-- | Five indices type
type I5 = (I, I, I, I, I)

-- | Coordinate type
type C = D
-- | Two coordinates type
type C2 = (C, C)
-- | Three coordinates type
type C3 = (C, C, C)
-- | Four coordinates type
type C4 = (C, C, C, C)
-- | Five coordinates type
type C5 = (C, C, C, C, C)

-- | 2D node containing two indices in assembled matrix
data XY = XY I2 C2

-- | XY node description
xy :: I2 -> C2 -> XY
xy = XY

-- | XY node freedom indices
xyFI :: XY -> I2
xyFI (XY i _) = i

-- | XY node coordinates
xyCoords :: XY -> C2
xyCoords (XY _ c) = c

-- | 3D node 
data XYC = XYC I3 C3

-- | XYC node description
xyc :: I3 -> C3 -> XYC
xyc = XYC

-- | XYC node freedom indices
xycFI :: XYC -> I3
xycFI (XYC i _) = i

-- | XYC node coordinates
xycCoords :: XYC -> C3
xycCoords (XYC _ c) = c

-- | 5D node containing five indices in assembled matrix
data XYZAB = XYZAB I5 C5

-- | XYZAB node description
xyzab :: I5 -> C5 -> XYZAB
xyzab = XYZAB

-- | XYZAB node freedom indices
xyzabFI :: XYZAB -> I5
xyzabFI (XYZAB i _) = i

-- | XYZAB node coordinates
xyzabCoords :: XYZAB -> C5
xyzabCoords (XYZAB _ c) = c
