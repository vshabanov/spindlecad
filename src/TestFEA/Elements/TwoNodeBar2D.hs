-- | 
-- Module      :  TestFEA.Elements.TwoNodeBar2D
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- 2D two-node bar finite element, currently only linear
--
module Elements.TwoNodeBar2D (
    twoNodeBar2D
    ) where

import Node
import Element
import ElementMatrix
import Material
import CrossSection

-- | 
twoNodeBar2D :: Node.XY -> Node.XY -> Material -> CrossSection -> E
twoNodeBar2D n1 n2 mat cs =
    linearElement
    (matrix 4 $ map (materialE mat * area cs / l *)
     [  c^2,  c*s, -c^2, -c*s
     ,  c*s,  s^2, -s*c, -s^2
     , -c^2, -s*c,  c^2,  s*c
     , -s*c, -s^2,  s*c,  s^2 ])
    (fi [i1,i2,i3,i4])
    where l  = sqrt $ dx^2 + dy^2
          dx = x2 - x1
          dy = y2 - y1
          c  = dx / l
          s  = dy / l
          (x1, y1) = xyCoords n1
          (x2, y2) = xyCoords n2
          (i1, i2) = xyFI n1
          (i3, i4) = xyFI n2
