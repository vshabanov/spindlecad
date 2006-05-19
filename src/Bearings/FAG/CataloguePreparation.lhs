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
--  along with Foobar; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
--

This module is used to process text catalogues and generate specific .lhs
modules.

> module CataloguePreperation where

> import System.IO
> import Control.Exception
> import Data.Char

> prepare =
>     bracket (openFile "SpindleBearingsCatalogue.txt" ReadMode) hClose $ \h -> do
>     c <- hGetContents h
>     let l = stripComments $ lines c
>         leftPage = filter (\ (x:_) -> isAlpha x) l
>         rightPage = filter (\ (x:_) -> isDigit x) l
>         concatenated = map
>             (\ (l,r) -> if head (words l) /= last (words r) then
>                           error ("Can't match " ++ head (words l))
>                         else
>                           "\"" ++ l ++ " " ++ escaped r ++ "\"")
>             $ zip leftPage rightPage
>     bracket (openFile "SpindleBearingsCatalogue.lhs" WriteMode) hClose $ \ o -> do
>     mapM_ (hPutStrLn o) copyrightNotice
>     hPutStrLn o ""
>     hPutStrLn o "This file is generated from SpindleBearingsCatalogue.txt."
>     hPutStrLn o "Use CataloguePreparation.prepare to update."
>     hPutStrLn o ""
>     hPutStrLn o "> module SpindleBearingsCatalogue where"
>     hPutStrLn o ""
>     hPutStrLn o "> list ="
>     hPutStr   o ">     ["
>     hPutStrLn o $ stringConcat ",\n>      " concatenated
>     hPutStrLn o ">     ]"

> stripComments = filter (\ x -> case x of
>                                    []    -> False
>                                    '-':_ -> False
>                                    _     -> True)

> escaped [] = []
> escaped (x:xs) | isAscii x = x : escaped xs
>                | otherwise = showLitChar x (escaped xs)

> stringConcat p [] = []
> stringConcat p [a] = a
> stringConcat p (x:xs) = x ++ p ++ stringConcat p xs

> copyrightNotice =
>     ["--",
>      "--  Copyright (C) 2006 Vladimir Shabanov",
>      "--",
>      "--  This file is part of SpindleCAD.",
>      "--",
>      "--  SpindleCAD is free software; you can redistribute it and/or modify",
>      "--  it under the terms of the GNU General Public License as published by",
>      "--  the Free Software Foundation; either version 2 of the License, or",
>      "--  (at your option) any later version.",
>      "--",
>      "--  SpindleCAD is distributed in the hope that it will be useful,",
>      "--  but WITHOUT ANY WARRANTY; without even the implied warranty of",
>      "--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
>      "--  GNU General Public License for more details.",
>      "--",
>      "--  You should have received a copy of the GNU General Public License",
>      "--  along with Foobar; if not, write to the Free Software",
>      "--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA",
>      "--"]
