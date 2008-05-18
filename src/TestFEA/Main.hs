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
                                   draw elements (fromIntegral w) (fromIntegral h)
                               return $ eventSent x
    tlt <- tooltipsNew
    tooltipsSetTip tlt canvas "Подсказка" ""
    tooltipsEnable tlt
    mainGUI
    
    --print $ elements
    --disp masterStiffness
    --print displacements
    print $ getv displacements 1
    print $ 1 / (getv displacements 1 * 1000)
  where n1 = xyc (1 , 2, 3) (0  ,0,0)
        n2 = xyc (4 , 5, 6) (100,0,0)
        n3 = xyc (7 , 8, 9) (124,0,0)
        n4 = xyc (10,11,12) (148,0,0)
        n5 = xyc (13,14,15) (900,0,0)
        cs = ring 0 100
        rigidity = 120000 -- N/mm
        elements =
            [
             bernoulliEulerBeam2D n1 n2 steel cs,
             bernoulliEulerBeam2D n2 n3 steel cs,
             bernoulliEulerBeam2D n3 n4 steel cs,
             bernoulliEulerBeam2D n4 n5 steel cs,
--              timoshenkoBeam2D n1 n2 steel cs,
--              timoshenkoBeam2D n2 n3 steel cs,
--              timoshenkoBeam2D n3 n4 steel cs,
--              timoshenkoBeam2D n4 n5 steel cs,
             linearYBearing n2 rigidity,
             linearYBearing n3 rigidity,
             linearYBearing n4 rigidity,
             linearYBearing n5 rigidity
            ]
        masterStiffness = assemble $ zip
                          (map stiffnessMatrix elements)
                          (map freedomIndices elements)
        masterForces = vector 15 ([0, 1] ++ replicate (15-2) 0)
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

draw :: [Element.E] -> Double -> Double -> Render ()
draw elements w h = withSavedMatrix $ do
    translate 0 (fromIntegral $ truncate $ h / 2)
    mapM_ (\ elt -> render elt [0..]) elements
    -- TODO: сделать передачу координат из displacements 
    -- по freedomIndices и сделать отрисовку BernoulliEulerBeam2d по
    -- её shape functions и посмотреть как отрисовать балку
    -- Тимошенко (должна показывать еще и поперечные смещения,
    -- какое-то дополнительное преобразование для shape functions или
    -- вообще другие shape functions?)

--     setSourceRGB 1 1 0
--     setLineWidth 5

--     moveTo 120 60
--     lineTo 60 110
--     lineTo 180 110
--     closePath
    
--     stroke

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
