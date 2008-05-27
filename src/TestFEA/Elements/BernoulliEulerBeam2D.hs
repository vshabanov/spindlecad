-- | 
-- Module      :  TestFEA.Elements.BernoulliEulerBeam2D
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- 2D plane Bernoulli-Euler beam, currently only linear element
--
module Elements.BernoulliEulerBeam2D (
    bernoulliEulerBeam2D,
    ) where

import Graphics.Rendering.Cairo
import Node
import Element
import ElementMatrix
import Material
import CrossSection

-- | Currently Y & C components must be equal to zero. Force on X axis
-- is ignored (1 in matrix diagonal).
-- Actually it's the same code as timoshenkoBeam2D except /f/
-- component is equal to zero.
bernoulliEulerBeam2D :: Node.XYC -> Node.XYC -> Material -> CrossSection -> E
bernoulliEulerBeam2D n1 n2 mat cs =
    linearElement
    (matrix 4 $ map (ei/(l^3*(1+f))*)
     [  12,         6*l,   -12,        6*l
     ,  6*l,  l^2*(4+f),  -6*l,  l^2*(2-f)
     ,  -12,       -6*l,    12,       -6*l
     ,  6*l,  l^2*(2-f),  -6*l,  l^2*(4+f) ])
    (fi [i2,i3,i5,i6])
    (drawBeam (x1, x2))
    where l  = abs $ x2 - x1
          ei = materialE mat * areaMomentOfInertia cs
          f = 0 -- 12 * ei / (materialG mat * timoshenko_A_s mat cs * l^2)                
          (x1, _, _) = xycCoords n1
          (x2, _, _) = xycCoords n2
          (i1, i2, i3) = xycFI n1
          (i4, i5, i6) = xycFI n2

drawBeam :: C2 -> RenderParameters -> [Node.C] -> Render ()
drawBeam (x1, x2) rp [dy1, dc1, dy2, dc2] = do
    let sc = (* displacementsScale rp)

    centerLine
    moveTo x1 0
    lineTo x2 0
    closePath
    stroke

    thickLine
    moveTo x1 $ sc dy1
    lineTo x2 $ sc dy2
    closePath
    stroke
    

centerLine :: Render ()
centerLine = do
    setSourceRGB 0.5 0.5 0.5
    setLineWidth 1
    setDash [20, 4] 0
    setLineCap LineCapRound

thickLine :: Render ()
thickLine = do
    setSourceRGB 0.3 0.3 0.3
    setLineWidth 2
    setDash [] 0
    setLineCap LineCapRound
