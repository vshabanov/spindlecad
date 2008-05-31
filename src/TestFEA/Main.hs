-- | 
-- Module      :  TestFEA.Main
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Stub 'do-nothing' main module
module Main (
    main
    ) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import System.Environment
import Data.Maybe (fromJust)
import Control.Monad
import ElementMatrix
import Material
import CrossSection
import Node
import Element
import Elements
 
-- | Main program
main :: IO ()
main = do
    initGUI
    canvas <- drawingAreaNew
    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

    mainWindow <- windowNew
    set mainWindow
            [windowDefaultWidth := 200, windowDefaultHeight := 200,
             containerChild := canvas, containerBorderWidth := 10,
             windowTitle := "SpindleCAD"]
    onDestroy mainWindow mainQuit

    widgetShowAll mainWindow -- чтобы появился drawWindow

    drawWindow <- widgetGetDrawWindow canvas
    onExpose canvas $ \x -> do (w,h) <- widgetGetSize canvas
                               renderWithDrawable drawWindow $
                                   draw scale
                                   elements
                                   (vectorToList displacements)
                                   (fromIntegral w) (fromIntegral h)
                               return $ eventSent x
--     tlt <- tooltipsNew
--     tooltipsSetTip tlt canvas "Подсказка" ""
--     tooltipsEnable tlt
    mainGUI
    
    --print $ elements
    --disp masterStiffness
    --print displacements
    print $ getv displacements 0
    print $ abs $ 1 / (getv displacements 0 * 1000)
  where u = undefined
        n1 = xyc (u, 0, 1) (  0, u, u)
        n2 = xyc (u, 2, 3) (100, u, u)
        n3 = xyc (u, 4, 5) (124, u, u)
        n4 = xyc (u, 6, 7) (148, u, u)
        n5 = xyc (u, 8, 9) (500, u, u)
        n6 = xyc (u,10,11) (900, u, u)
--         cs = ring 0 100
--         rigidity = 120000 -- N/mm
--         scale = 3*10000000
        cs = ring 0 600
        rigidity = 200*120000 -- N/mm
        scale = 500*10000000
        elements =
            [
             bernoulliEulerBeam2D n1 n2 steel cs,
             bernoulliEulerBeam2D n2 n3 steel cs,
             bernoulliEulerBeam2D n3 n4 steel cs,
             bernoulliEulerBeam2D n4 n5 steel cs,
             bernoulliEulerBeam2D n5 n6 steel cs,
--              timoshenkoBeam2D n1 n2 steel cs,
--              timoshenkoBeam2D n2 n3 steel cs,
--              timoshenkoBeam2D n3 n4 steel cs,
--              timoshenkoBeam2D n4 n5 steel cs,
             linearYBearing n2 rigidity,
             linearYBearing n3 rigidity,
             linearYBearing n4 rigidity,
             linearYBearing n6 rigidity
            ]
        masterStiffness = assemble $ zip
                          (map stiffnessMatrix elements)
                          (map freedomIndices elements)
        masterForces = vector 12 ([-1] ++ replicate (12-1) 0)
        displacements = solve masterStiffness masterForces

space = 10

hbox :: [Widget] -> IO Widget
hbox children = do
    b <- hBoxNew True space
    flip mapM_ children $ \ widget -> boxPackStart b widget PackGrow 0
    return $ toWidget b

vbox :: [Widget] -> IO Widget
vbox children = do
    b <- vBoxNew True space
    flip mapM_ children $ \ widget -> boxPackStart b widget PackGrow 0
    return $ toWidget b

draw :: Double -> [Element.E] -> [Node.C] -> Double -> Double -> Render ()
draw sc elements displacements w h = withSavedMatrix $ do
    let indexedDisplacements = zip [0..] displacements
        eltDisp elt = map (fromJust . flip lookup indexedDisplacements)
                      $ freedomIndices elt
        renderParameters = RenderParameters
                           { displacementsScale = sc
                           }
    translate 100 (fromIntegral $ truncate $ h / 2)
    scale 1 (-1) -- flip Y
    -- setAntialias AntialiasSubpixel
    mapM_ (\ elt -> render elt renderParameters (eltDisp elt)) elements
    -- TODO: сделать передачу координат из displacements 
    -- по freedomIndices и сделать отрисовку BernoulliEulerBeam2d по
    -- её shape functions и посмотреть как отрисовать балку
    -- Тимошенко (должна показывать еще и поперечные смещения,
    -- какое-то дополнительное преобразование для shape functions или
    -- вообще другие shape functions?)

withSavedMatrix :: Render () -> Render ()
withSavedMatrix r = do
    m <- getMatrix
    r
    setMatrix m

-- a4 :: Num a => (a, a)
-- a4 = (210, 297)

-- writePDF :: IO ()
-- writePDF =
--     withPDFSurface "myDraw.pdf" pdw pdh $
--         \s -> renderWith s $ do myDraw pdw pdh
--                                 showPage
--     where pdw = fst a4 / 25.4 * 72
--           pdh = snd a4 / 25.4 * 72
