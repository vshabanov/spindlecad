> import Numeric.LinearAlgebra hiding (scale)
> import Text.Printf

-- > import Data.Array.IArray
-- > import Data.Array.Diff

-- > type Matrix e = DiffArray Int (DiffArray Int e)

-- > matrix :: [[e]] -> Matrix e
-- > matrix l = listArray (0, length rows - 1) rows
-- >     where rows = map (\ r -> listArray (0, length r - 1) r) l

Немного байды для работы с матрицами

> import Control.Monad.ST
> import Data.Array.ST

> type E = Double
> type STMatrix s = STArray s Int (STArray s Int E)

> stMatrixOfList :: [[E]] -> ST s (STMatrix s)
> stMatrixOfList l = do rows <- mapM (\ r -> newListArray (1, length r) r) l
>                       newListArray (1, length rows) rows

> stMatrix :: Numeric.LinearAlgebra.Matrix E -> ST s (STMatrix s)
> stMatrix = stMatrixOfList . toLists

> laMatrix :: STMatrix s -> ST s (Numeric.LinearAlgebra.Matrix E)
> laMatrix m = do stRows <- getElems m
>                 rows <- mapM getElems stRows
>                 return $ fromLists rows

> readSTMatrix :: Int -> Int -> STMatrix s -> ST s E
> readSTMatrix c r m = do row <- readArray m r
>                         e <- readArray row c
>                         return e

> modifySTMatrix :: Int -> Int -> (E -> E) -> STMatrix s -> ST s ()
> modifySTMatrix c r f m = do row <- readArray m r
>                             e <- readArray row c
>                             writeArray row c (f e)


> disp m = putStrLn $ format "  " (printf "%.2f") m
> dispv v = disp $ fromColumns [v]


Тупо повторяем на хаскеле
Analysis of Example Truss by a CAS. 
http://www.colorado.edu/engineering/CAS/courses.d/IFEM.d/IFEM.Ch04.d/IFEM.Ch04.index.html

Матрица жесткости двухмерной пружины

> elemStiff3DTwoNodeBar (x1,y1) (x2,y2) (e,a) =
>     e*a/l .* (4><4)[  c^2,  c*s, -c^2, -c*s
>                    ,  c*s,  s^2, -s*c, -s^2
>                    , -c^2, -s*c,  c^2,  s*c
>                    , -s*c, -s^2,  s*c,  s^2 :: Double ]
>     where l = sqrt $ dx^2 + dy^2
>           dx = x2 - x1
>           dy = y2 - y1
>           c = dx / l
>           s = dy / l


Тест
*Main> disp $ elemStiff3DTwoNodeBar (0,0) (10,10) (100, 2*sqrt 2)
 10.00   10.00  -10.00  -10.00
 10.00   10.00  -10.00  -10.00
-10.00  -10.00   10.00   10.00
-10.00  -10.00   10.00   10.00


Слияние матрицы жесткости элемента в глобальную матрицу жесткости

> mergeElemIntoMasterStiff keIn eftab kIn =
>     runST $ do k <- stMatrix kIn
>                ke <- stMatrix keIn
>                for $ \ (i, fi) ->
>                    for $ \ (j, fj) ->
>                        do e <- readSTMatrix i j ke
>                           modifySTMatrix fi fj (+ e) k
>                laMatrix k
>   where for f = mapM_ f $ zip [1..] eftab


Сборка фермы из примера

> assembleMasterStiffOfExampleTruss =
>     foldl (\ k (ke, eftab) -> mergeElemIntoMasterStiff ke eftab k)
>           kZero kes
>   where kes = [(elemStiff3DTwoNodeBar ( 0, 0) (10, 0) (100,        1), [1,2,3,4]),
>                (elemStiff3DTwoNodeBar (10, 0) (10,10) (100,      1/2), [3,4,5,6]),
>                (elemStiff3DTwoNodeBar ( 0, 0) (10,10) (100, 2*sqrt 2), [1,2,5,6])]
>         kZero = 0 .* ident 6 -- как, блин, сразу нулевую матрицу создать?
>                              -- (6><6) [0, 0 ..] почему-то виснет (hmatrix что-то форсит?)

*Main> disp assembleMasterStiffOfExampleTruss
 20.00   10.00  -10.00   0.00  -10.00  -10.00
 10.00   10.00    0.00   0.00  -10.00  -10.00
-10.00    0.00   10.00   0.00    0.00    0.00
  0.00    0.00    0.00   5.00    0.00   -5.00
-10.00  -10.00    0.00   0.00   10.00   10.00
-10.00  -10.00    0.00  -5.00   10.00   15.00
