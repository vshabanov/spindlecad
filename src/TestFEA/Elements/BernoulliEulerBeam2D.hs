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
import Debug.Trace

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
    (drawBeam cs (x1, x2))
    where l  = abs $ x2 - x1
          ei = materialE mat * areaMomentOfInertia cs
          f = 0 -- 12 * ei / (materialG mat * timoshenko_A_s mat cs * l^2)                
          (x1, _, _) = xycCoords n1
          (x2, _, _) = xycCoords n2
          (i1, i2, i3) = xycFI n1
          (i4, i5, i6) = xycFI n2

drawBeam :: CrossSection -> C2 -> RenderParameters -> [Node.C] -> Render ()
drawBeam cs (x1, x2) rp bold_u = do
    let sc  = (* displacementsScale rp)
        l   = abs $ x2 - x1
        -- TODO: взять матрицу для балки Тимошенко, проверить,
        -- сделать BernoulliEulerBeam2D частным случаем (materialG=0)
        h_b = matrix 4 $ map (/60) [  30,  5*l,   30, -5*l
                                   , -36, -3*l,   36, -3*l
                                   ,   0, -5*l,    0,  5*l
                                   ,   6,  3*l,   -6,  3*l
                                   ]
        -- | Generalized coordinates [c_1, c_2, c_3, c_4]
        c :: [D]
        c   = vectorToList $ h_b `mulmv` vector 4 bold_u
        -- | Natural coordinate e <- [-1..1] = 2*x/l - 1
        e x = 2*(x-x1)/(x2-x1) - 1
        -- | Legendre polynomials (u interpolation)
        legendre e = [ 1, e, 1/2*(3*e^2-1), 1/2*(5*e^3-3*e) ]
        -- | First derivative with respect to x of Legendre
        -- polynomials (v interpolation)
        legendre' e = [ 0, 2/l, 6*e/l, 3*(5*e^2-1)/l ]
        -- | Interpolation in generalized coordinates
        interpolate l x = sum $ zipWith (*) c (l $ e x)
        -- | Displacements field
        u x y = -y * theta x
        v x y = interpolate legendre x
        theta x = interpolate legendre'
        -- TODO: искажается диаметр балки при больших масштабах смещений
        -- причем и в верхнем случае (из книжки) и в нижнем (навскидку)
        -- надо разобраться
--         u x y = -y * sin (theta x)
--         v x y = interpolate legendre x - y * (1 - cos (theta x))
--         theta x = atan $ interpolate legendre' x
        -- | Displaced coodrinates
        xy x y = (sc (u x y) + x, sc (v x y) + y)

--     trace (concat [show bold_u, "\n",
--                    show c, "\n",
--                    show $ legendre (-1), "\n",
--                    show $ v x1 0, "\n"]) centerLine

    centerLine
    moveTo x1 0
    lineTo x2 0
    stroke

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
        rIn = dIn cs / 2
        rOut = dOut cs / 2

    thickLine
    yLine 0
    -- TODO: проверить численно осевую линию на совпадение
    yLine rOut
    yLine (-rOut)
    if rIn > 0
       then do yLine rIn
               yLine (-rIn)
       else return ()

    xLine x1 rIn rOut
    xLine x2 (-rIn) (-rOut)
    

centerLine :: Render ()
centerLine = do
    setSourceRGB 0.5 0.5 0.5
    setLineWidth 1
    setDash [15, 2, 4, 2] 0
    setLineCap LineCapRound

thickLine :: Render ()
thickLine = do
    setSourceRGB 0.3 0.3 0.3
    setLineWidth 2
    setDash [] 0
    setLineCap LineCapRound
