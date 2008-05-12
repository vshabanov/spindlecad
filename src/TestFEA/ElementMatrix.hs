-- | 
-- Module      :  TestFEA.ElementMatrix
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Finite element matrices utilities.
--
module ElementMatrix (
    -- * Types
    D,
    M,
    V,
    I,
    FI,
    -- * Functions
    -- ** Creation
    matrix, vector, fi,
    -- ** Printing
    disp, dispv,
    -- ** Assembly
    assemble,
    -- ** Solution
    solve    
    ) where

import Numeric.LinearAlgebra as LA
import Text.Printf
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

-- | Matrix element type.
type D = Double

-- | Finite element matrix.
type M = LA.Matrix D

-- | Finite element vector (forces, boundary conditions).
type V = LA.Vector D

-- | Index type
type I = Int

-- | Freedom indices list
type FI = [I]

-- | Makes fresh square finite element matrix.
matrix :: Int -> [D] -> M
matrix n e = (n><n) e

-- | Makes fresh vector.
vector :: Int -> [D] -> V
vector n e = n |> e

-- | Makes freedom indices list.
fi :: (Integral a, Num a) => [a] -> FI
fi x = map fromIntegral x

-- | Display matrix.
disp :: M -> IO ()
disp m = putStrLn $ format "  " (printf "%.2f") m

-- | Display vector.
dispv :: V -> IO ()
dispv v = disp $ fromColumns [v]

-- | Linear solution.
solve :: M -> V -> V
solve = (<\>)

-- | Element matrices assembling.
-- Uses list of matrices and degrees of freedom indices to assemble
-- master matrix.
assemble :: [(M, FI)] -> M
assemble kes = runST $ do
    k <- zeroSquareMatrix size
    m <- mapM (\ (ke, eftab) -> stMatrix ke >>= \ s -> return (s, eftab)) kes
    assemble' m k
    laMatrix k
  where size = maximum $ map (\ (_, eftab) -> maximum eftab) kes

zeroSquareMatrix :: Int -> ST s (STMatrix s)
zeroSquareMatrix n = stMatrixOfList $ take n $ repeat (take n $ repeat 0)

assemble' :: [(STMatrix s, [Int])] -> STMatrix s -> ST s ()
assemble' kes k =
    mapM_ (\ (ke, eftab) -> mergeElemIntoAssembly' ke eftab k) kes

mergeElemIntoAssembly' :: STMatrix s -> [Int] -> STMatrix s -> ST s ()
mergeElemIntoAssembly' ke eftab k =
    for $ \ (i, fi) ->
        for $ \ (j, fj) ->
            do e <- readSTMatrix i j ke
               modifySTMatrix fi fj (+ e) k
    where for f = mapM_ f $ zip [1..] eftab

-- Некоторые утилиты для работы с ST матрицами (в них удобнее
-- оперировать на уровне элементов, чем с матрицами
-- из Numeric.LinearAlgebra)

type STMatrix s = STArray s Int (STArray s Int D)

-- Тут не получилось STUArray т.к. он не может хранить в себе другой STUArray,
-- а только примитивный тип. Т.е. если хочется STUArray то надо делать его
-- одномерным и хранить кол-во байт в строке.

stMatrixOfList :: [[D]] -> ST s (STMatrix s)
stMatrixOfList l = do rows <- mapM (\ r -> newListArray (1, length r) r) l
                      newListArray (1, length rows) rows

stMatrix :: M -> ST s (STMatrix s)
stMatrix = stMatrixOfList . toLists

laMatrix :: STMatrix s -> ST s M
laMatrix m = do stRows <- getElems m
                rows <- mapM getElems stRows
                return $ fromLists rows

-- laVector :: STMatrix s -> ST s (LA.Vector D)
-- laVector m = laMatrix m >>= (head . toColumns)

readSTMatrix :: Int -> Int -> STMatrix s -> ST s D
readSTMatrix c r m = do row <- readArray m r
                        e <- readArray row c
                        return e

modifySTMatrix :: Int -> Int -> (D -> D) -> STMatrix s -> ST s ()
modifySTMatrix c r f m = do row <- readArray m r
                            e <- readArray row c
                            writeArray row c (f e)

writeSTMatrix :: Int -> Int -> D -> STMatrix s -> ST s ()
writeSTMatrix c r e m = modifySTMatrix c r (\ _ -> e) m