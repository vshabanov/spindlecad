-- | Главный модуль -- russian letters doesn't work in Haddock
-- Copyright (c) 2008 Vladimir Shabanov
-- GPL version 2 or later
module Main (
    main
    ) where

import System.Environment
import ElementMatrix
 
-- | Main program
main :: IO ()
main = getArgs >>= print
