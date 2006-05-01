This module contains CAS expression type and some utility functions
to operate with.

> module CASExpr where

> import Data.Ratio
> import Data.IORef
> import qualified Data.Map as Map

Abstract CAS expression.
The CAS expression is an abstraction layer over any maxima-like CAS system.
CAS expression can be converted to maxima lisp and vice versa.
It is the CAS expression with which the user works.

TODO: redesign CASExpr to make it extensible (is this really necessary now?)
For the moment extension is done through Funcall. Previously Funcall has
string argument for functions but its too dangerous. Now it has CASFunction
sum type as parameter and extension can only be done in this module.
But other CAS systems can provide other functionality.
We'll back to this when start constraint solver
or (less possible) Axiom interface.
Rigidity is better than anarchy in this case.

Thoughts:
  We can create separate types for expressions and define cas functions
  through special interface, e.g.
    solve = eqlist **-> symlist **-> eqlistlist
    diff = expr **-> symbol **-> expr
  But implementation details are not clear for the moment.

> data CASExpr = Integer Integer
>              | Rational Rational
>              | Symbol String
>              | String String
>              | Plus CASExpr CASExpr  -- maybe replace this stuff with BinOp?
>              | Minus CASExpr CASExpr
>              | Multiply CASExpr CASExpr
>              | Divide CASExpr CASExpr
>              | Expt CASExpr CASExpr
>              | Equal CASExpr CASExpr
>              | List [CASExpr]
>              | Funcall CASFunction [CASExpr]
>                deriving (Eq, Show)

Predefined CAS functions. We use maxima protocol by default.
All other systems must follow this protocol for compability

> data CASFunction = CFSolve  -- eqlist -> symlist -> varvallist
>                  | CFDiff   -- expr -> symbol [-> int] -> expr
>                  | CFSubst  -- varvallist -> expr -> expr
>                  | CFAbs
>                  | CFSignum
>                  | CFExp
>                  | CFLog
>                  | CFSqrt
>                  | CFSin
>                  | CFCos
>                  | CFTan
>                  | CFASin
>                  | CFACos
>                  | CFATan
>                  | CFSinh
>                  | CFCosh
>                  | CFTanh
>                  | CFASinh
>                  | CFACosh
>                  | CFATanh
>                    deriving (Eq, Ord, Show)

Some mathematical constants expressed as predefined symbols.
Concrete CAS interface implementation must substitute these symbols
with CAS-specific ones.

> cas_pi = Symbol "_cas_pi"
> cas_e  = Symbol "_cas_e"

Some utility

> solve eqlist unknowns =
>     Funcall CFSolve [List eqlist, List $ map Symbol unknowns]

> diff expr what = Funcall CFDiff [expr, Symbol what]
> diffn expr what n = Funcall CFDiff [expr, Symbol what, Integer n]

> subst varValPairs expr =
>     Funcall CFSubst [List (map (\ (var, val) -> Symbol var `Equal` val)
>                            varValPairs),
>                      expr]

CASExpr evaluation for specified num type.
We can use any Floating number type to calculate results.
Here some examples (Era.CR is from /src/ThirdParty):

eval (sin pi) :: Float          => -8.742278e-8
eval (sin pi) :: Double         => 1.2246063538223773e-16
eval (sin pi) :: Era.CR         => 0.0000000000000000000000000000000000000000
eval (log cas_e) :: Float       => 0.99999994
eval (log cas_e) :: Double      => 1.0
eval (log cas_e) :: Era.CR      => 1.0000000000000000000000000000000000000000
eval pi                         => 3.141592653589793
eval pi :: Era.CR               => 3.1415926535897932384626433832795028841972

Remark that Double is a default.

> eval :: (Num a, Fractional a, Floating a) => CASExpr -> a
> eval (Integer i) = fromInteger i
> eval (Rational r) = fromRational r
> eval (Plus a b) = eval a + eval b
> eval (Minus a b) = eval a - eval b
> eval (Multiply a b) = eval a * eval b
> eval (Divide a b) = eval a / eval b
> eval (Expt a b) = eval a ** eval b
> eval e@(Funcall f [a]) =
>     (case f of
>         CFAbs -> abs
>         CFSignum -> signum
>         CFExp -> exp
>         CFLog -> log
>         CFSqrt -> sqrt
>         CFSin -> sin
>         CFCos -> cos
>         CFTan -> tan
>         CFASin -> sin
>         CFACos -> cos
>         CFATan -> tan
>         CFSinh -> sinh
>         CFCosh -> cosh
>         CFTanh -> tanh
>         CFASinh -> asinh
>         CFACosh -> acosh
>         CFATanh -> atanh
>         _ -> error ("CASExpr.eval not supported for: " ++ show e)
>      ) (eval a)
> eval e = if e == cas_pi then pi
>          else if e == cas_e then exp 1
>          else error ("CASExpr.eval not supported for: " ++ show e)

CASExpr symbol substitution (with evaluation or Integer/Rational afterwards)

> substitute :: Map.Map String CASExpr -> CASExpr -> CASExpr
> substitute m = subst
>     where subst (Symbol s)     = Map.findWithDefault (Symbol s) s m
>           subst (Plus a b)     = e Plus a b
>           subst (Minus a b)    = e Minus a b
>           subst (Multiply a b) = e Multiply a b
>           subst (Divide a b)   = e Divide a b
>           subst (Expt a b)     = e Expt a b
>           subst (Equal a b)    = e Equal a b
>           subst (List a)       = List $ map subst a
>           subst (Funcall s a)  = Funcall s $ map subst a
>           subst a              = a 
>           e op a b = simpleEval $ op (subst a) (subst b)

simple non-recursive evaluation of Integer/Rational forms

> simpleEval :: CASExpr -> CASExpr
> simpleEval = se
>     where se (Plus a b)       = bo (+) (+) a b Plus
>           se (Minus a b)      = bo (-) (-) a b Minus
>           se (Multiply a b)   = bo (*) (*) a b Multiply
>           se (Divide (Integer a) (Integer b)) = sr (a%b)
>           se (Divide a b)     = bo' (/) a b Divide
>           se (Expt (Integer a)  (Integer b))  =
>               if b >= 0 then Integer $ a ^ b else sr $ r a ^^ b
>           se (Expt (Rational a) (Integer b))  = sr $ a ^^ b
>           se (List l)                         = List $ map se l
>           se (Funcall f l)                    = Funcall f $ map se l
>           se a                                = a
>           -- binary operation trying
>           bo iop rop (Integer a)  (Integer b)  d = Integer $ iop a b
>           bo iop rop a b  d = bo' rop a b d 
>           bo' rop (Integer a)  (Rational b) d = sr $ rop (r a) b
>           bo' rop (Rational a) (Integer b)  d = sr $ rop a (r b)
>           bo' rop (Rational a) (Rational b) d = sr $ rop a b
>           bo' rop a b def                     = def a b
>           r = toRational
>           sr r = simplifyRational $ Rational r

> simplifyRational (Rational r) =
>     if denominator r == 1 then Integer (numerator r) else Rational r

Haskell numeric classes for CASExpr.

Num

> instance Num CASExpr where
>     a + b = simpleEval (Plus a b)
>     a - b = simpleEval (Minus a b)
>     a * b = simpleEval (Multiply a b)
>             
>     negate (Integer i) = Integer $ negate i
>     negate (Rational r) = simplifyRational $ Rational $ negate r
>     negate (Minus (Integer 0) a) = a
>     negate (Minus a b) = Minus b a
>     negate a = 0 - a
>
>     abs a    = Funcall CFAbs [a]
>     signum a = Funcall CFSignum [a]
>                
>     fromInteger i = Integer i

Fractional

> instance Fractional CASExpr where
>     a / b = simpleEval (Divide a b)
>             
>     fromRational r =
>         if denominator r == 1 then Integer (numerator r) else Rational r

Floating

> instance Floating CASExpr where
>     pi        = cas_pi
>     exp a     = Funcall CFExp [a]
>     log a     = Funcall CFLog [a]
>     sqrt a    = Funcall CFSqrt [a]
>     a ** b    = Expt a b
>     -- logBase x y  = log y / log x
>     sin a     = Funcall CFSin [a]
>     cos a     = Funcall CFCos [a]
>     tan a     = Funcall CFTan [a]
>     asin a    = Funcall CFASin [a]
>     acos a    = Funcall CFACos [a]
>     atan a    = Funcall CFATan [a]
>     sinh a    = Funcall CFSinh [a]
>     cosh a    = Funcall CFCosh [a]
>     tanh a    = Funcall CFTanh [a]
>     asinh a   = Funcall CFASinh [a]
>     acosh a   = Funcall CFACosh [a]
>     atanh a   = Funcall CFATanh [a]


TODO: rework ordering and enumerations, they are work in simple cases
like [0..10]::[CASExpr], but error in something containing symbols. 
Hmm. maybe its OK?

Ord

> instance Ord CASExpr where
>     compare x y | ex == ey  = EQ
>                 | ex <= ey  = LT
>                 | otherwise = GT
>         where ex = eval x
>               ey = eval y


Enum

> numericEnumFrom n            = n : (numericEnumFrom $! (n+1))
> numericEnumFromThen n m      = iterate ((m-n)+) n
> numericEnumFromTo n m        = takeWhile (<= m) (numericEnumFrom n)
> numericEnumFromThenTo n n' m = takeWhile p (numericEnumFromThen n n')
>                                where p | n' >= n   = (<= m)
>                                        | otherwise = (>= m)

> instance Enum CASExpr where
>     succ x            = x+1
>     pred x            = x-1
>     toEnum x          = Integer $ toInteger x
>     fromEnum x        = fromInteger $ truncate $ eval x  -- may overflow
>     enumFrom          = numericEnumFrom
>     enumFromThen      = numericEnumFromThen
>     enumFromTo n m    = numericEnumFromTo n (m+1/2)
>     enumFromThenTo n n' m = numericEnumFromThenTo n n' (m + (n'-n)/2)
