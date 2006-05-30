--
--  Copyright (C) 2006 Vladimir Shabanov
--
--  This file is part of SpindleCAD.
--
--  SpindleCAD is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  SpindleCAD is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with SpindleCAD; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
--

Primitive 2D drawing description data type & simple exporting to ACAD lisp.

> module Drawing (
>     Drawing(..),
>     Point(..),
>     LineStyle(..),
>     exportToACAD, -- :: Drawing -> FilePath -> IO () / exportToACAD "file.lsp"
>     mirrorX, mirrorY,
>     centeredText,
>     move, over, scale,
>     changeLineStyleTo,
>     substituteDrawing,
>     filletLine
>   ) where

> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList
> import CASExpr (eval, substitute, inexactEq, inexactCompare)
> import Lisp hiding (Value)
> import qualified Lisp (Value)
> import System.IO
> import Control.Exception


Drawing data type.

> data Drawing = EmptyDrawing
>              | Line LineStyle [Point]
>              | Spline LineStyle [Point]
>              | Circle LineStyle Point (Value Meter)
>              | Arc LineStyle Point (Value Meter) Angle Angle
>              | Text Point String -- simple centered multi-line text
>              | Over Drawing Drawing
>                deriving (Eq, Ord, Show)


Point data type. Remark that we give it in physical units,
i.e. no pixels - vector graphics only.

> data Point = Point (Value Meter) (Value Meter)
>              deriving (Eq, Ord, Show)

> data LineStyle = NormalLine
>                | CenterLine
>                | ThinLine
>                | HiddenLine
>                  deriving (Eq, Ord, Show)

Drawing editing

> data Editor = Editor { pointMap :: (Value Meter, Value Meter)
>                                 -> (Value Meter, Value Meter),
>                        arcAnglesMap :: (Angle, Angle) -> (Angle, Angle),
>                        lineStyleMap :: LineStyle -> LineStyle 
>                      }

> defaultEditor = Editor { pointMap = id,
>                          arcAnglesMap = id,
>                          lineStyleMap = id 
>                        }

Text utility

> centeredText x y s = Text (Point x y) s

Mirror drawing relative x-axis, y-axis

> mirrorX = mapDrawing (defaultEditor
>                       { pointMap = (\ (x, y) -> (x, (-1).*y)),
>                         arcAnglesMap = (\ (a1, a2) -> (-a2, -a1)) 
>                       })

> mirrorY = mapDrawing (defaultEditor
>                       { pointMap = (\ (x, y) -> ((-1).*x, y)),
>                         arcAnglesMap = (\ (a1, a2) -> (pi-a2, pi-a1))
>                       })

Drawing moving

> move dx dy = mapDrawing (defaultEditor
>                          { pointMap = (\ (x, y) -> (x+.dx, y+.dy))
>                          })

Drawing scaling

> scale kx ky = mapDrawing (defaultEditor
>                          { pointMap = (\ (x, y) -> (kx.*x, ky.*y))
>                          })

`over` utility - the same as Over constructor but ignores EmptyDrawing

> EmptyDrawing `over` EmptyDrawing = EmptyDrawing
> EmptyDrawing `over` d            = d
> d            `over` EmptyDrawing = d
> d1           `over` d2           = d1 `Over` d2

> infixl 6  `over` -- same fixity as +

Line style changing

> changeLineStyleTo l = mapDrawing (defaultEditor { lineStyleMap = (\ _ -> l) 
>                                                 })

Substituting of symbols which can appear in drawing description

> substituteDrawing m =
>     mapDrawing (Editor { pointMap = (\ (x,y) ->(s x, s y)),
>                          arcAnglesMap  = (\ (a1,a2) ->(sa a1, sa a2)),
>                          lineStyleMap = id 
>                        })
>     where s = substitutepv m
>           sa = substitute m

Drawing mapping

> mapDrawing :: Editor -> Drawing -> Drawing
> mapDrawing e d = mapd d
>     where mapd EmptyDrawing = EmptyDrawing
>           mapd (Line ls p) = Line (l ls) (points p)
>           mapd (Spline ls p) = Spline (l ls) (points p)
>           mapd (Circle ls p r) = Circle (l ls) (point p) r
>           mapd (Arc ls p r a1 a2) =
>               let (a1',a2') = (arcAnglesMap e) (a1,a2) in
>               Arc (l ls) (point p) r a1' a2'
>           mapd (Text p s) = Text (point p) s
>           mapd (Over d1 d2) = Over (mapd d1) (mapd d2)
>
>           l = lineStyleMap e
>           points = map (liftPoint (pointMap e))
>           point = liftPoint (pointMap e)
>           liftPoint f = (\ (Point x y) -> let (nx,ny) = f (x,y) in
>                          Point nx ny)


Fillet line.

> filletLine (x1:y1:x2:y2:r:x3:y3:xs) =
>     let (l1,a,l2) = fillet (Line NormalLine [Point x1 y1, Point x2 y2])
>                            (Line NormalLine [Point x2 y2, Point x3 y3])
>                            NormalLine r
>     --let (l1,l2) = (Line NormalLine [Point x1 y1, Point x2 y2],
>     --               Line NormalLine [Point x2 y2, Point x3 y3])
>         Line _ [Point xb yb, _] = l2
>     in
>       l1 `Over` a `Over` (if xs == [] then l2 else filletLine (xb:yb:x3:y3:xs))
>

fillet now works only for horizontal/vertical lines

> fillet (Line ls1 [Point x1 y1, Point x2 y2])
>        (Line ls2 [Point _  _ , Point x3 y3]) ls r =
>            (Line ls1 [Point x1 y1, Point xa ya],
>             Arc ls (if l1horiz then Point xa yb else Point xb ya) r
>                    (a1*degree) (a2*degree),
>             Line ls2 [Point xb yb, Point x3 y3])
>     where xa = x2 -. (signum $ (x2-.x1)/.meter).*r
>           ya = y2 -. (signum $ (y2-.y1)/.meter).*r
>           xb = x2 +. (signum $ (x3-.x2)/.meter).*r
>           yb = y2 +. (signum $ (y3-.y2)/.meter).*r
>           l1horiz = inexactEq (y1/.meter) (y2/.meter)
>           (>.) a b = inexactCompare (a/.meter) (b/.meter) == GT
>           (a1, a2) = if l1horiz
>                      then
>                        if yb >. y1
>                           then (if x2>.x1 then (-90,0) else (180,-90))
>                           else (if x2>.x1 then (0,90) else (90,180))
>                      else 
>                        if xb >. x1
>                           then (if y2>.y1 then (90,180) else (180,-90))
>                           else (if y2>.y1 then (0,90) else (-90,0))


AutoCAD lisp file exporting.

> exportToACAD :: Drawing -> FilePath -> IO ()
> exportToACAD d s = bracket (openFile s WriteMode) hClose $ \ h -> do
>     let putCommands = mapM_ (\ l -> hPutLisp h l >> hPutStrLn h "")
>     putCommands layersSetup
>     -- turn off all snapping, since we draw precisely
>     putCommands [scommand "orthomode" ["0"],
>                  scommand "polarmode" ["0"],
>                  scommand "snapmode" ["0"],
>                  scommand "osnap" [""]]
>     putCommands $ drawingCommands d
>     


Call AutoCAD command:
(apply 'command '("cmd") params))

> command cmd params =
>     List [Symbol "apply", Quote (Symbol "command"),
>           Quote (List (String cmd : params))]

> scommand cmd params = command cmd (map String params)


Work with layers.

(command "layer" "new" "Hidden" "")             -- new layer
(command "layer" "Ltype" "HIDDEN" "Hidden" "")  -- setting line type
(command "layer" "LWeight" "0.35" "Hidden" "")  -- setting line weight

(command "layer" "set" "Hidden" "")             -- selection

> layer2 name cmd        = scommand "layer" [cmd, name, ""]
> layer3 name cmd param  = scommand "layer" [cmd, param, name, ""]

> selectLayer name = layer2 name "set"

> layersSetup :: [Lisp.Value]
> layersSetup =
>     [layer2 "Normal" "new",
>      layer3 "Normal" "Ltype" "CONTINUOUS",
>      layer3 "Normal" "LWeight" "0.5",
>      
>      layer2 "Center lines" "new",
>      layer3 "Center lines" "Ltype" "CENTER",
>      layer3 "Center lines" "LWeight" "0.25",
>      
>      layer2 "Hidden lines" "new",
>      layer3 "Hidden lines" "Ltype" "HIDDEN",
>      layer3 "Hidden lines" "LWeight" "0.35",
>      
>      layer2 "Thin lines" "new",
>      layer3 "Thin lines" "Ltype" "CONTINUOUS",
>      layer3 "Thin lines" "LWeight" "0.25"
>     ]


Work with drawings

Line:
(command "line" '(0 0) '(100 100) "")
                                  ^^ indicate drawing stop
(apply 'command (append '("line") points '("")))

Spline:
(command "spline" '(0 0) '(100 100) "" "" "")
(apply 'command (append '("spline") points '("" "" "")))

Circle:
(command "circle" '(0 0) 100)

Arc:
(command "arc" "c" `(x y) `(x+r*sin(a1) y+r*cos(a1))
                          `(x+r*sin(a2) y+r*cos(a2)))

Drawing to commands list conversion.

> drawingCommands :: Drawing -> [Lisp.Value]
> drawingCommands (Line ls p) = [lineStyle ls,
>                                command "line" (points p ++ [dstop])]
> drawingCommands (Spline ls p) = [lineStyle ls,
>                                  command "spline" (points p ++
>                                                    [dstop, dstop, dstop])]
> drawingCommands (Circle ls c r) = [lineStyle ls,
>                                    command "circle" [point c, value r]]
> drawingCommands (Arc ls (Point x y) r a1 a2) =
>     [lineStyle ls,
>      command "arc" [String "c",
>                     point (Point x y),
>                     point (Point (x+.cos a1.*r) (y+.sin a1.*r)),
>                     point (Point (x+.cos a2.*r) (y+.sin a2.*r))]]
> drawingCommands (Text p s) =
>     [command "mtext" ([point p,
>                        String "j",
>                        String "MC", -- middle-center justification
>                        point p] -- same center point
>                       ++
>                       map (\ l -> String (if l == "" then " " else l))
>                         (lines s)
>                       ++
>                       [String ""])]
> drawingCommands (Over d1 d2 ) =
>     drawingCommands d2
>     ++
>     drawingCommands d1

Drawing-to-commands utilities.

> lineStyle NormalLine = selectLayer "Normal"
> lineStyle CenterLine = selectLayer "Center lines"
> lineStyle HiddenLine = selectLayer "Hidden lines"
> lineStyle ThinLine   = selectLayer "Thin lines"

> points p = map point p

> point (Point x y) = List [Double (eval (x /. mm)), Double (eval (y /. mm))]

> value v = Double (eval (v /. mm))

> angle a = Double (eval (a/degree))

> dstop = String ""
