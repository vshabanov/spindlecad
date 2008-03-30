> import Graphics.UI.Gtk
> import Graphics.Rendering.Cairo

> main :: IO ()
> main = do
>   initGUI
>   window <- windowNew
>   onDestroy window mainQuit
>   button <- toggleButtonNew -- WithLabel "Кнопка"
>   boxDefault   <- labelBox "favicon.ico" "SpindleCAD"
>   boxMouseOver <- labelBox "favicon.ico" "SpindleCAD???"
>   onEnter button $ clearContainer button >> containerAdd button boxMouseOver >> widgetShowAll window
>   onLeave button $ clearContainer button >> containerAdd button boxDefault   >> widgetShowAll window
>   --containerAdd button boxDefault
>   set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
>               -- containerChild := button, containerBorderWidth := 10,
>               windowTitle := "SpindleCAD"]
>                   
>   canvas <- drawingAreaNew
>   containerAdd window canvas
>   widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
>                  
>   widgetShowAll window -- чтобы появился drawWindow
>                 
>   drawWindow <- widgetGetDrawWindow canvas
>   onExpose canvas (\x -> do (w,h) <- widgetGetSize canvas
>                             renderWithDrawable drawWindow $
>                                 myDraw (fromIntegral w) (fromIntegral h)
>                             return (eventSent x))
>                  
>   tlt <- tooltipsNew
>   tooltipsSetTip tlt button "Подсказка" ""
>   tooltipsEnable tlt
>   mainGUI

> clearContainer c = containerForeach c (containerRemove c)

Боксик с картинкой и текстом

> labelBox :: FilePath -> String -> IO HBox
> labelBox fn txt = do
>   box   <- hBoxNew False 0
>   set box [containerBorderWidth := 2]
>   image <- imageNewFromFile fn
>   label <- labelNew (Just txt)
>   boxPackStart box image PackNatural 3
>   boxPackStart box label PackNatural 3
>   return box

> myDraw :: Double -> Double -> Render ()
> myDraw w h = do
>     setSourceRGB 1 1 0
>     setLineWidth 5
>
>     moveTo 120 60
>     lineTo 60 110
>     lineTo 180 110
>     closePath
>     
>     stroke

> writePDF =
>     withPDFSurface "myDraw.pdf" pdw pdh $
>         \s ->  renderWith s $ do myDraw pdw pdh
>                                  showPage
>     where pdw = 3*72
>           pdh = 2*72
