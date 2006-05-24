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
>     Drawing,
>     Point,
>     LineStyle,
>     exportToACAD -- :: Drawing -> FilePath -> IO () / exportToACAD "file.lsp"
>   ) where

> import TypeLevelPhysicalDimension
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList
> import CASExpr (eval)
> import Lisp hiding (Value)
> import qualified Lisp (Value)
> import System.IO
> import Control.Exception


Drawing data type.

> data Drawing = Line LineStyle [Point]
>              | Spline LineStyle [Point]
>              | Circle LineStyle Point (Value Meter)
>              | Arc LineStyle Point (Value Meter) Angle Angle
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


AutoCAD lisp file exporting.

> exportToACAD :: Drawing -> FilePath -> IO ()
> exportToACAD d s = bracket (openFile s WriteMode) hClose $ \ h -> do
>     mapM_ (hPutStrLn h . show) layersSetup
>     mapM_ (hPutStrLn h . show) $ drawingCommands d


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
(command "ellipse" "a" '(x (y+r)) '(x (y-r)) '((x+r) y)
                        (startAngle-90) (endAngle-90))

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
>      command "ellipse" [String "a",
>                         point (Point x (y +. r)),
>                         point (Point x (y -. r)),
>                         point (Point (x +. r) y),
>                         angle (a1 - 90*degree),
>                         angle (a2 - 90*degree)]]
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
