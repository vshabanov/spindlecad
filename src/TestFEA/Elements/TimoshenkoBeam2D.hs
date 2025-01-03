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

import Graphics.Rendering.Cairo
import Drawing
import Node hiding (xy)
import qualified Element
import ElementMatrix
import Material
import CrossSection
-- import Debug.Trace

-- | Currently Y & C components must be equal to zero. Force on X axis
-- is ignored (1 in matrix diagonal).
-- Actually it's the same code as timoshenkoBeam2D except /f/
-- component is equal to zero.
timoshenkoBeam2D :: Node.XYC -> Node.XYC -> Material -> CrossSection -> Element.E
timoshenkoBeam2D n1 n2 mat cs =
    Element.linear
    (matrix 4 $ map (ei/(l^3*(1+f))*)
     [   12 ,        6*l ,   -12 ,        6*l
     ,   6*l,   l^2*(4+f),   -6*l,   l^2*(2-f)
     ,  -12 ,       -6*l ,    12 ,       -6*l
     ,   6*l,   l^2*(2-f),   -6*l,   l^2*(4+f) ])
    (freedomIndices [i2,i3,i5,i6])
    (drawBeam f cs (x1, x2))
    where l  = abs $ x2 - x1
          ei = materialE mat * areaMomentOfInertia cs
          f  = 12 * ei / (materialG mat * timoshenko_A_s mat cs * l^2)                
          (x1, _, _) = xycCoords n1
          (x2, _, _) = xycCoords n2
          (_i1, i2, i3) = xycFI n1
          (_i4, i5, i6) = xycFI n2

drawBeam :: D -> CrossSection -> C2 -> Element.RenderParameters -> [Node.C] -> Render ()
drawBeam f cs (x1, x2) rp bold_u = do
    let sc  = (* Element.displacementsScale rp)
        l   = abs $ x2 - x1
        h_s = matrix 4 $ map (/60) [   30,  5*l,   30, -5*l
                                   , -f36, -f3l,  f36, -f3l
                                   ,    0, -5*l,    0,  5*l
                                   ,   f6,  f3l,  -f6,  f3l
                                   ]
              where f3l = 3*l/(1+f)
                    f36 = (36 + 30*f)/(1+f)
                    f6  = 6/(1+f)
        -- | Generalized coordinates [c_1, c_2, c_3, c_4]
        c :: [D]
        c   = vectorToList $ h_s `mulmv` vector 4 (map sc bold_u)
        -- | Natural coordinate \xi <- [-1..1] = 2*x/l - 1
        xi x = 2*(x-x1)/(x2-x1) - 1
        -- | Legendre polynomials (u interpolation)
        legendre e = [ 1, e, 1/2*(3*e^2-1), 1/2*(5*e^3-3*e) ]
        -- | First derivative with respect to x of Legendre
        -- polynomials (v interpolation)
        legendre' e = [ 0, 2/l, 6*e/l, 3*(5*e^2-1)/l ]
        -- | Interpolation in generalized coordinates
        interpolate poly x = sum $ zipWith (*) c (poly $ xi x)
        -- | Displacements field
        u x y = -y * sin (theta x) 
        v x y = interpolate legendre x + y * (cos (theta x) - 1)
        dy = interpolate legendre'
        theta x = atan $ dy x + gamma
        gamma = 10 * f / l * c!!3
        -- | Displaced coodrinates
        xy x y = (u x y + x, v x y + y)

--     trace (concat [show $ map sc bold_u, "\n",
--                    show c, "\n",
--                    show gamma, "\n",
--                    show $ v x1 0, "\n"]) centerLine

    let yLine y =
            do uncurry moveTo $ xy x1 y
               flip mapM_ [x1, x1+1 .. x2] $ \ x ->
                   uncurry lineTo $ xy x y
               uncurry lineTo $ xy x2 y
               stroke
        xLine x y1 y2 =
            do uncurry moveTo $ xy x y1
               uncurry lineTo $ xy x y2
               stroke
--                uncurry moveTo $ kasat x (-50)
--                uncurry lineTo $ kasat x ( 50)
--                stroke
--         kasat x dx = (x + dx, (snd $ xy x 0) + dy x * dx)
        rIn = dIn cs / 2
        rOut = dOut cs / 2
        sectionDistance :: (Num a) => a
        sectionDistance = 10
        trunc :: Double -> Double
        trunc x = fromIntegral (truncate x `div` sectionDistance) * sectionDistance
        sections = [trunc x1, trunc x1 + sectionDistance .. x2]

    -- axis
    centerLine
    moveTo x1 0
    lineTo x2 0
    stroke

    -- center displacements
    ultraLightLine
    flip mapM_ sections $ \ x ->
        do moveTo x 0
           uncurry lineTo $ xy x 0
           stroke

    -- cross sections
    lightLine
    flip mapM_ sections $ \ x ->
        do xLine x rIn rOut
           xLine x (-rIn) (-rOut)

    -- deformed beam contour
    thickLine
    yLine 0
    yLine (-rOut)
    yLine rOut
    -- TODO: проверить численно осевую линию на совпадение
    if rIn > 0
       then do yLine (-rIn)
               yLine rIn
       else return ()

    xLine x1 (-rOut) rOut
    xLine x2 (-rOut) rOut
