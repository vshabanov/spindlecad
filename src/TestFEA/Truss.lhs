> import Numeric.LinearAlgebra as LA
> import Text.Printf

-- > import Data.Array.IArray
-- > import Data.Array.Diff

-- > type Matrix e = DiffArray Int (DiffArray Int e)

-- > matrix :: [[e]] -> Matrix e
-- > matrix l = listArray (0, length rows - 1) rows
-- >     where rows = map (\ r -> listArray (0, length r - 1) r) l

Немного байды для работы с матрицами

> import Control.Monad
> import Control.Monad.ST
> import Data.Array.ST

> type E = Double
> type STMatrix s = STArray s Int (STArray s Int E)

Тут не получилось STUArray т.к. он не может хранить в себе другой STUArray,
а только примитивный тип. Т.е. если хочется STUArray то надо делать его
одномерным и хранить кол-во байт в строке.

> -- | Ф-я
> stMatrixOfList :: [[E]] -> ST s (STMatrix s)
> stMatrixOfList l = do rows <- mapM (\ r -> newListArray (1, length r) r) l
>                       newListArray (1, length rows) rows

> stMatrix :: LA.Matrix E -> ST s (STMatrix s)
> stMatrix = stMatrixOfList . toLists

> laMatrix :: STMatrix s -> ST s (LA.Matrix E)
> laMatrix m = do stRows <- getElems m
>                 rows <- mapM getElems stRows
>                 return $ fromLists rows

-- > laVector :: STMatrix s -> ST s (LA.Vector E)
-- > laVector m = laMatrix m >>= (head . toColumns)

> readSTMatrix :: Int -> Int -> STMatrix s -> ST s E
> readSTMatrix c r m = do row <- readArray m r
>                         e <- readArray row c
>                         return e

> modifySTMatrix :: Int -> Int -> (E -> E) -> STMatrix s -> ST s ()
> modifySTMatrix c r f m = do row <- readArray m r
>                             e <- readArray row c
>                             writeArray row c (f e)

> writeSTMatrix :: Int -> Int -> E -> STMatrix s -> ST s ()
> writeSTMatrix c r e m = modifySTMatrix c r (\ _ -> e) m


> disp m = putStrLn $ format "  " (printf "%.2f") m
> dispv v = disp $ fromColumns [v]


Тупо повторяем на хаскеле
Analysis of Example Truss by a CAS. 
http://www.colorado.edu/engineering/CAS/courses.d/IFEM.d/IFEM.Ch04.d/IFEM.Ch04.index.html

Матрица жесткости двухмерной пружины

> elemStiff2DTwoNodeBar :: (E,E) -> (E,E) -> (E,E) -> Matrix E
> elemStiff2DTwoNodeBar (x1,y1) (x2,y2) (e,a) =
>     e*a/l .* (4><4)[  c^2,  c*s, -c^2, -c*s
>                    ,  c*s,  s^2, -s*c, -s^2
>                    , -c^2, -s*c,  c^2,  s*c
>                    , -s*c, -s^2,  s*c,  s^2 :: E ]
>     where l = sqrt $ dx^2 + dy^2
>           dx = x2 - x1
>           dy = y2 - y1
>           c = dx / l
>           s = dy / l


Тест
*Main> disp $ elemStiff2DTwoNodeBar (0,0) (10,10) (100, 2*sqrt 2)
 10.00   10.00  -10.00  -10.00
 10.00   10.00  -10.00  -10.00
-10.00  -10.00   10.00   10.00
-10.00  -10.00   10.00   10.00


Слияние матрицы жесткости элемента в глобальную матрицу жесткости

ke -- матрица жесткости элемента
eftab -- таблица индексов степеней свободы
  (индексы координат в глобальной матрице жесткости)
k -- глобальная матрица жесткости

> mergeElemIntoMasterStiff' :: STMatrix s -> [Int] -> STMatrix s -> ST s ()
> mergeElemIntoMasterStiff' ke eftab k =
>     for $ \ (i, fi) ->
>         for $ \ (j, fj) ->
>             do e <- readSTMatrix i j ke
>                modifySTMatrix fi fj (+ e) k
>   where for f = mapM_ f $ zip [1..] eftab

> mergeElemIntoMasterStiff :: Matrix E -> [Int] -> Matrix E -> Matrix E
> mergeElemIntoMasterStiff keIn eftab kIn =
>     runST $ do k <- stMatrix kIn
>                ke <- stMatrix keIn
>                mergeElemIntoMasterStiff' ke eftab k
>                laMatrix k


Сборка глобальной матрицы жесткости

> zeroSquareMatrix :: Int -> ST s (STMatrix s)
> zeroSquareMatrix n = stMatrixOfList $ take n $ repeat (take n $ repeat 0)

> assembleMasterStiff' :: [(STMatrix s, [Int])] -> STMatrix s -> ST s ()
> assembleMasterStiff' kes k =
>     mapM_ (\ (ke, eftab) -> mergeElemIntoMasterStiff' ke eftab k) kes

> assembleMasterStiff :: [(Matrix E, [Int])] -> Matrix E
> assembleMasterStiff kes =
>     runST $ do k <- zeroSquareMatrix size
>                m <- mapM (\ (ke, eftab) -> stMatrix ke >>= \ s -> return (s, eftab)) kes
>                assembleMasterStiff' m k
>                laMatrix k
>   where size = maximum $ map (\ (_, eftab) -> maximum eftab) kes


Модификация глобальной матрицы жесткости для учета граничных условий.
(граничными условиями смещений (displacement boundary conditions, DBC) здесь
могут быть только нулевые смещения)

k -- матрица жесткости
f -- вектор сил
pdof -- зафиксированные (prescribed) степени свободы

> modifiedMasterStiffForcesForDBC :: (Matrix E, Vector E) -> [Int] -> (Matrix E, Vector E)
> modifiedMasterStiffForcesForDBC (k, f) pdof = runST $
>     do kmod <- stMatrix k
>        (_, nk) <- getBounds kmod
>        fmod <- stMatrix $ fromColumns [f]
>        flip mapM_ pdof $ \ i ->
>            do flip mapM_ [1..nk] $ \ j ->
>                   writeSTMatrix j i (if j /= i then 0 else 1) kmod
>               writeSTMatrix 1 i 0 fmod
>        liftM2 (,) (laMatrix kmod) (laMatrix fmod >>= (return . head . toColumns))


Сборка фермы из примера

> exampleMasterStiff :: Matrix E
> exampleMasterStiff =
>     assembleMasterStiff
>     [(elemStiff2DTwoNodeBar ( 0, 0) (10, 0) (100,        1), [1,2,3,4]),
>      (elemStiff2DTwoNodeBar (10, 0) (10,10) (100,      1/2), [3,4,5,6]),
>      --(elemStiff2DTwoNodeBar (10, 0) ( 0,10) (100,      1/2), [5,6,7,8]),
>      (elemStiff2DTwoNodeBar ( 0, 0) (10,10) (100, 2*sqrt 2), [1,2,5,6])]

*Main> disp exampleMasterStiff
 20.00   10.00  -10.00   0.00  -10.00  -10.00
 10.00   10.00    0.00   0.00  -10.00  -10.00
-10.00    0.00   10.00   0.00    0.00    0.00
  0.00    0.00    0.00   5.00    0.00   -5.00
-10.00  -10.00    0.00   0.00   10.00   10.00
-10.00  -10.00    0.00  -5.00   10.00   15.00

> exampleForces :: Vector E
> exampleForces = 6 |> [undefined, undefined, 0, undefined, 2, 1]
>     where undefined = 1/0

> examplePDofs :: [Int]
> examplePDofs = [1,2,4]

> (exampleModifiedMasterStiff, exampleModifiedForces) =
>     modifiedMasterStiffForcesForDBC
>     (exampleMasterStiff, exampleForces) examplePDofs

> solvedExampleDisplacements = exampleModifiedMasterStiff <\> exampleModifiedForces

*Main> dispv solvedExampleDisplacements
 0.00
 0.00
 0.00
 0.00
 0.40
-0.20

> solvedExampleForces = exampleMasterStiff <> solvedExampleDisplacements

*Main> dispv solvedExampleForces
-2.00
-2.00
-0.00
 1.00
 2.00
 1.00
