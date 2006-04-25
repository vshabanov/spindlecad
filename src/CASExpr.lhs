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
For the moment extension is done through ugly Funcall and the CASExpr itself
is a sum type which is not good for extension since other CAS systems can
provide other functionality. We'll back to this when start constraint solver
or (less possible) Axiom interface.

TODO: redesign CASExpr.
Current troubles with CASExpr:
  - Too much boilerplate code (see below, hm its better to don't)
    It come from:
      - several representations of number (we need exactly one - infinite
        precision numbers)
      - many different cases for binary operations (it doesn't add any syntax
        clearness, but odd to handle)
  - No type checking at all
  - Zero extensibility (funcall is ugly and doesn't grow - it will cause
    too much errors)
  - Too hard to create expressions
    (maybe from string using parser, but again - no type checking)

Thoughts:
  We can create separate types for expressions and define cas functions
  through special interface, e.g.
    solve = eqlist **-> symlist **-> eqlistlist
    diff = expr **-> symbol **-> expr
  But implementation details are not clear for the moment.

> data CASExpr = Integer Integer
>              | Rational Rational
>              | Double Double
>              | Symbol String
>              | String String
>              | Plus CASExpr CASExpr  -- maybe replace this stuff with BinOp?
>              | Minus CASExpr CASExpr
>              | Multiply CASExpr CASExpr
>              | Divide CASExpr CASExpr
>              | Expt CASExpr CASExpr
>              | Equal CASExpr CASExpr
>              | List [CASExpr]
>              | Funcall String [CASExpr] -- its better to exclude this at all!
>                deriving (Eq, Ord, Show)

TODO: add CASExpr evaluation
eval :: CASExpr -> CASExpr

CASExpr symbol substitution (with evaluation afterwards)

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

simple non-recursive evaluation

> simpleEval :: CASExpr -> CASExpr
> simpleEval = se
>     where se (Plus a b)       = bo (+) (+) (+) a b Plus
>           se (Minus a b)      = bo (-) (-) (-) a b Minus
>           se (Multiply a b)   = bo (*) (*) (*) a b Multiply
>           se (Divide (Integer a) (Integer b)) = Rational (a%b)
>           se (Divide a b)     = bo' (/) (/) a b Divide
>           se (Expt (Integer a)  (Integer b))  =
>               if b >= 0 then Integer $ a ^ b else Rational $ r a ^^ b
>           se (Expt (Integer a)  (Rational b)) = Double $ di a ** dr b
>           se (Expt (Integer a)  (Double b))   = Double $ di a ** b
>           se (Expt (Rational a) (Integer b))  = Rational $ a ^^ b
>           se (Expt (Rational a) (Rational b)) = Double $ dr a ** dr b
>           se (Expt (Rational a) (Double b))   = Double $ dr a ** b
>           se (Expt (Double a)   (Integer b))  = Double $ a ** di b
>           se (Expt (Double a)   (Rational b)) = Double $ a ** dr b
>           se (Expt (Double a)   (Double b))   = Double $ a ** b
>           se (List l)                         = List $ map se l
>           se (Funcall f l)                    = Funcall f $ map se l
>           se a                                = a
>           -- binary operation trying
>           bo iop rop dop (Integer a)  (Integer b)  d = Integer $ iop a b
>           bo iop rop dop a b  d = bo' rop dop a b d 
>           bo' rop dop (Integer a)  (Rational b) d = Rational $ rop (r a) b
>           bo' rop dop (Integer a)  (Double b)   d = Double $ dop (di a) b
>           bo' rop dop (Rational a) (Integer b)  d = Rational $ rop a (r b)
>           bo' rop dop (Rational a) (Rational b) d = Rational $ rop a b
>           bo' rop dop (Rational a) (Double b)   d = Double $ dop (dr a) b
>           bo' rop dop (Double a)   (Integer b)  d = Double $ dop a (di b)
>           bo' rop dop (Double a)   (Rational b) d = Double $ dop a (dr b)
>           bo' rop dop (Double a)   (Double b)   d = Double $ dop a b
>           bo' rop dop a b def                     = def a b
>           r = toRational
>           di = fromRational . toRational
>           dr = fromRational
