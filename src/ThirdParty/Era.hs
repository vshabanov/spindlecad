-- ERA: Exact Real Arithmetic (version 1.0)
--
-- A tolerably efficient and possibly correct implementation of the computable
-- reals using Haskell 1.2.
--
-- David Lester, Department of Computer Science, Manchester University, M13 9PL.
--           (2000-2001)

module ThirdParty.Era where

import Ratio
import Char
import Numeric (readDec, readSigned)

data CR = CR_ (Int -> Integer)

instance Eq  CR where
  x == y = s' (digitsToBits digits) == 0 where (CR_ s') = x-y
instance Ord CR where
  x <= y = s' (digitsToBits digits) <= 0 where (CR_ s') = x-y
  x <  y = s' (digitsToBits digits) <  0 where (CR_ s') = x-y
  x >= y = s' (digitsToBits digits) >= 0 where (CR_ s') = x-y
  x >  y = s' (digitsToBits digits) >  0 where (CR_ s') = x-y
  max (CR_ x') (CR_ y') = CR_ (\p -> max (x' p) (y' p))
  min (CR_ x') (CR_ y') = CR_ (\p -> min (x' p) (y' p))

instance Num CR where
  (CR_ x') + (CR_ y') = CR_ (\p -> round_uk ((x' (p+2) + y' (p+2))%4))
  (CR_ x') * (CR_ y') = CR_ (\p -> round_uk ((x' (p+sy)*y' (p+sx))%2^(p+sx+sy)))
                        where x0 = abs (x' 0)+2; y0 = abs (y' 0)+2
                              sx = sizeinbase x0 2+3; sy = sizeinbase y0 2+3
  negate (CR_ x')     = CR_ (\p -> negate (x' p))
  abs x               = max x (negate x)
  signum (CR_ x')     = fromInteger (signum (x' (digitsToBits digits)))
  fromInteger n       = CR_ (\p -> n*2^p)

instance Fractional CR where
  recip (CR_ x') = CR_ (\p -> let s = head [n | n <- [0..], 3 <= abs (x' n)]
                              in round_uk (2^(2*p+2*s+2)%(x' (p+2*s+2))))
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)
  x/y = x*recip y

-- two useful scaling functions:

div2n :: CR -> Int -> CR
div2n (CR_ x') n = CR_ (\p -> if p >= n then x' (p-n) else round_uk (x' p%2^n))

mul2n :: CR -> Int -> CR
mul2n (CR_ x') n = CR_ (\p -> x' (p+n))

-- transcendental functions (mostly range reductions):

instance Floating CR where
  pi = fromInteger 16 * atan (fromRational (1%5)) 
                - fromInteger 4 * atan (fromRational (1%239))
  sqrt x  = CR_ (\p -> floorsqrt (x' (2*p))) where (CR_ x') = x

  log x   = if t < 0 then error "log of negative number\n" else
            if t < 4 then - log (recip x)                  else
            if t < 8 then log_dr x                         else
            {- 7 < t -}   log_dr (div2n x n) + fromInteger (toInteger n) * log2
            where (CR_ x') = x; t = x' 2; n = sizeinbase t 2 - 3
  exp x   = if n < 0 then div2n (exp_dr s) (fromInteger (-n)) else
            if n > 0 then mul2n (exp_dr s) (fromInteger n) else exp_dr s
            where (CR_ u') = x/log2; n = u' 0; s = x-fromInteger n*log2
  sin x   = if n == 0 then sin_dr y                           else
            if n == 1 then sqrt1By2 * (cos_dr y + sin_dr y)   else
            if n == 2 then cos_dr y                           else
            if n == 3 then sqrt1By2 * (cos_dr y - sin_dr y)   else
            if n == 4 then - sin_dr y                         else
            if n == 5 then - sqrt1By2 * (cos_dr y + sin_dr y) else
            if n == 6 then - cos_dr y                         else
            {- n == 7 -}   - sqrt1By2 * (cos_dr y - sin_dr y)
            where (CR_ z') = x/piBy4; s = round_uk (z' 2%4); n = s `mod` 8
                  y = x - piBy4 * fromInteger s
  cos x   = if n == 0 then cos_dr y                           else
            if n == 1 then sqrt1By2 * (cos_dr y - sin_dr y)   else
            if n == 2 then sin_dr y                           else
            if n == 3 then sqrt1By2 * (cos_dr y + sin_dr y)   else
            if n == 4 then - cos_dr y                         else
            if n == 5 then - sqrt1By2 * (cos_dr y - sin_dr y) else
            if n == 6 then - sin_dr y                         else
            {- n == 7 -}   - sqrt1By2 * (cos_dr y + sin_dr y)
            where (CR_ z') = x/piBy4; s = round_uk (z' 2%4); n = s `mod` 8
                  y = x - piBy4 * fromInteger s
  atan x  = if t <  -5 then atan_dr (negate (recip x)) - piBy2 else
            if t == -4 then -piBy4 - atan_dr (xp1/xm1)         else
            if t <   4 then atan_dr x                          else
            if t ==  4 then piBy4 + atan_dr (xm1/xp1)          else
            {- t >   4 -}   piBy2 - atan_dr (recip x)
            where (CR_ x') = x; t = x' 2
                  xp1 = x+fromInteger 1; xm1 = x-fromInteger 1
  asin x  = if x0 >  0 then pi / fromInteger 2 - atan (s/x) else
            if x0 == 0 then atan (x/s)                      else
            {- x0 <  0 -}   atan (s/x) - pi / fromInteger 2
            where (CR_ x') = x; x0 = x' 0; s = sqrt (fromInteger 1 - x*x)
  acos x  = pi / fromInteger 2 - asin x
  sinh x  = (y - recip y) / fromInteger 2 where y = exp x
  cosh x  = (y + recip y) / fromInteger 2 where y = exp x
  tanh x  = (y - y') / (y + y') where y = exp x; y' = recip y
  asinh x = log (x + sqrt (x*x + fromInteger 1))
  acosh x = log (x + sqrt (x*x - fromInteger 1))
  atanh x = log ((fromInteger 1 + x) / (fromInteger 1 - x)) / fromInteger 2


acc_seq :: (Rational -> Integer -> Rational) -> [Rational]
acc_seq f = scanl f (1%1) [1..]

exp_dr :: CR -> CR
exp_dr = power_series (acc_seq (\a n -> a*(1%n))) id

log_dr :: CR -> CR
log_dr x = y * log_drx y where y = (x - fromInteger 1) / x

log_drx :: CR -> CR
log_drx = power_series [1%n | n <- [1..]] (+1)

sin_dr :: CR -> CR
sin_dr x = x*power_series (acc_seq (\a n -> -a*(1%(2*n*(2*n+1))))) id (x*x)

cos_dr :: CR -> CR
cos_dr x = power_series (acc_seq (\a n -> -a*(1%(2*n*(2*n-1))))) id (x*x)

atan_dr :: CR -> CR
atan_dr x = (x/y) * atan_drx ((x*x)/y) where y = x*x+fromInteger 1

atan_drx :: CR -> CR
atan_drx = power_series (acc_seq (\a n -> a*((2*n)%(2*n+1)))) (+1)

-- power_series takes as arguments:
--   a (rational) list of the coefficients of the power series
--   a function from the desired accuracy to the number of terms needed
--   the argument x

power_series :: [Rational] -> (Int -> Int) -> CR -> CR
power_series ps terms (CR_ x')
  = CR_ (\p -> let t = terms p; l2t = 2*sizeinbase (toInteger t+1) 2+6; p' = p + l2t
                   xr = x' p'; xn = 2^p'; g xn = round_uk ((xn*xr)%(2^p'))
               in round_uk (accumulate (iterate g xn) (take t ps) % (2^l2t)))
    where accumulate _      []     = 0
          accumulate (x:xs) (c:cs) = let t = round_uk (c*(x%1)) in
                                     if t == 0 then 0 else t + accumulate xs cs

-- Some useful constants:

piBy2 :: CR
piBy2 = div2n pi 1

piBy4 :: CR
piBy4 = div2n pi 2

log2 :: CR
log2 = div2n (log_drx (recip (fromInteger 2))) 1

sqrt1By2 :: CR
sqrt1By2 = sqrt (recip (fromInteger 2))

instance Enum CR where
  enumFrom         = iterate (+ fromInteger 1)
  enumFromThen n m = iterate (+(m-n)) n
  
instance Real CR
 -- where toRational x@(CR_ x') = x' n % 2^n where n = digitsToBits digits
instance RealFrac CR where
  properFraction x@(CR_ x') = (fromInteger n, x - fromInteger n) where n = x' 0
instance RealFloat CR

-- printing and reading the reals:

get_str :: Int -> CR -> String
get_str d (CR_ x')
  = (if s then "-" else "") ++ zs ++ (if d /= 0 then '.':fs else "")
    where b  = digitsToBits d
          n  = x' b
          ds = show (round_uk ((n*10^d)%2^b))
          (s,ds') = let s = head ds == '-' in (s, if s then tail ds else ds)
          ds'' = take (max (d+1-length ds') 0) (repeat '0') ++ ds'
          (zs,fs) = splitAt (length ds'' -d) ds''

digitsToBits :: Int -> Int
digitsToBits d = ceiling (fromIntegral d * (logBase 2.0 10.0)) + 4

digits :: Int
digits = 40

instance Read CR where
  readsPrec p = readSigned readFloat

instance Show CR where
  showsPrec p x = let xs = get_str digits x in
                  if head xs == '-' then showParen (p > 6) (showString xs)
                                    else showString xs

-- GMP functions not provided by Haskell

sizeinbase :: Integer -> Int -> Int
sizeinbase n b = f (abs n)
                 where f n = if n <= 1 then 1 else 1 + f (n `div` toInteger b)

floorsqrt :: Integer -> Integer
floorsqrt x = until satisfy improve x
              where improve y = floor ((y*y+x)%(2*y))
                    satisfy y = y*y <= x && x <= (y+1)*(y+1)

round_uk :: Rational -> Integer
round_uk x = floor (x+1%2)

-- type ReadS a = String -> [(a, String)]

readFloat :: (RealFloat a) => ReadS a
readFloat r = [(fromRational ((n%1)*10^^(k-d)),t) | (n,d,s) <- readFix r,
                                                    (k,t) <- readExp s]
              where readFix r = [(read (ds++ds'), length ds', t)
                                        | (ds,'.':s) <- lexDigits r,
                                          (ds',t)    <- lexDigits s ]
                    readExp (e:s) | e `elem` "eE" = readExp' s
                    readExp s                     = [(0,s)]
                    readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
                    readExp' ('+':s) = readDec s
                    readExp' s       = readDec s

lexDigits :: ReadS String
lexDigits = nonnull isDigit

nonnull :: (Char -> Bool) -> ReadS String
nonnull p s = [(cs,t) | (cs@(_:_),t) <- [span p s]]
