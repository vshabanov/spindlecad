-- | 
-- Module      :  TestFEA.Elements.TimoshenkoBeam2D
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- 2D plane timoshenko beam, currently only linear element
--
module Elements.TimoshenkoBeam2D (
    timoshenkoBeam2D,
    ) where

import Node
import Element
import ElementMatrix
import Material
import CrossSection

-- | Currently Y & C components must be equal to zero. Force on X axis
-- is ignored (1 in matrix diagonal).
timoshenkoBeam2D :: Node.XYC -> Node.XYC -> Material -> CrossSection -> E
timoshenkoBeam2D n1 n2 mat cs =
    linearElement
    (matrix 6 $ map (ei/(l^3*(1+f))*)
     [   1,    0,         0,  0,    0,         0
     ,   0,   12,       6*l,  0,  -12,       6*l
     ,   0,  6*l, l^2*(4+f),  0, -6*l, l^2*(2-f)
     ,   0,    0,         0,  1,    0,         0
     ,   0,  -12,      -6*l,  0,   12,      -6*l
     ,   0,  6*l, l^2*(2-f),  0, -6*l, l^2*(4+f) ])
    (fi [i1,i2,i3,i4,i5,i6])
    (\ x -> return ())
    where l  = abs $ x2 - x1
          ei = materialE mat * areaMomentOfInertia cs
          f = 12 * ei / (materialG mat * timoshenko_A_s mat cs * l^2)
          (x1, _, _) = xycCoords n1
          (x2, _, _) = xycCoords n2
          (i1, i2, i3) = xycFI n1
          (i4, i5, i6) = xycFI n2
