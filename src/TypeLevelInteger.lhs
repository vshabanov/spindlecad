> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level integers.

> module TypeLevelInteger where

We implement sum type using separate data types for each contrustor
and type class for the whole sum. Data types are made instances
of this type class. So we get type level sum type.

    data Peano = One | Succ Peano
    data Z = Neg Peano | Zero | Pos Peano 

Peano numbers data contructors and type class:

> data One    = One    deriving (Eq, Ord, Show)
> data Succ a = Succ a deriving (Eq, Ord, Show)

> class Peano x
> instance Peano One
> instance Peano n => Peano (Succ n)

Integer data contructors and type class:

> data Zero  = Zero  deriving (Eq, Ord, Show)
> data Pos a = Pos a deriving (Eq, Ord, Show)
> data Neg a = Neg a deriving (Eq, Ord, Show)

> class Z x
> instance Z Zero
> instance Peano n => Z (Pos n)
> instance Peano n => Z (Neg n)


On the next we implement arithmetic operations on peano and integer numbers
at type level.

Function level code for each function is presented as idented block before
each type level function definition. You can see that the type information
is presented as type class and each pattern match is done through the
corresponding class instance.

    peanoPlus :: Peano -> Peano -> Peano
    peanoPlus One      b = Succ b
    peanoPlus (Succ a) b = Succ (peanoPlus a b)

> class (Peano a, Peano b, Peano c) => PeanoPlus a b c | a b -> c where
>     peanoPlus :: a -> b -> c

> instance Peano b => PeanoPlus One b (Succ b) where
>     peanoPlus One b = Succ b
> instance PeanoPlus a b c => PeanoPlus (Succ a) b (Succ c) where
>     peanoPlus (Succ a) b = Succ (peanoPlus a b)

    peanoMultiply :: Peano -> Peano -> Peano
    peanoMultiply One b = b
    peanoMultiply (Succ a) b = peanoPlus b (peanoMultiply a b)

> class (Peano a, Peano b, Peano c) => PeanoMultiply a b c | a b -> c where
>     peanoMultiply :: a -> b -> c

> instance Peano b => PeanoMultiply One b b where
>     peanoMultiply One b = b
> instance (PeanoMultiply a b c, PeanoPlus b c d) => PeanoMultiply (Succ a) b d where
>     peanoMultiply (Succ a) b = peanoPlus b (peanoMultiply a b)

    peanoMinus :: Peano -> Peano -> Z
    peanoMinus  One      One     = Zero            -- 1 - 1 = 0
    peanoMinus  One     (Succ b) = Neg b           -- 1 - (1+b) = -b
    peanoMinus (Succ a)  One     = Pos a           -- (1+a) - 1 = a
    peanoMinus (Succ a) (Succ b) = peanoMinus a b  -- (1+a) - (1+b) = a -b 

> class (Peano a, Peano b, Z c) => PeanoMinus a b c | a b -> c where
>     peanoMinus :: a -> b -> c

> instance PeanoMinus One One Zero where
>     peanoMinus One One = Zero
> instance Peano b => PeanoMinus One (Succ b) (Neg b) where
>     peanoMinus One (Succ b) = Neg b
> instance Peano a => PeanoMinus (Succ a) One (Pos a) where
>     peanoMinus (Succ a) One = Pos a
> instance (PeanoMinus a b c) => PeanoMinus (Succ a) (Succ b) c where
>     peanoMinus (Succ a) (Succ b) = peanoMinus a b

    peanoDiv :: Peano -> Peano -> Z
    peanoDiv a b = peanoDiv' b Zero a b

> class (PeanoDiv' b Zero a b c) => PeanoDiv a b c | a b -> c where
>     peanoDiv :: a -> b -> c

> instance (PeanoDiv' b Zero a b c) => PeanoDiv a b c where
>     peanoDiv a b = peanoDiv' b Zero a b

    peanoDiv' :: Peano -> Z -> Peano -> Peano -> Z
    peanoDiv' d acc One One = acc `zplus` Pos One
    peanoDiv' d acc One (Succ b) = acc
    peanoDiv' d acc (Succ a) One = peanoDiv' d (acc `zplus` Pos One) a d
    peanoDiv' d acc (Succ a) (Succ b) = peanoDiv' d acc a b

> class (Peano d, Z acc, Peano a, Peano b, Z c) => PeanoDiv' d acc a b c | d acc a b -> c where
>     peanoDiv' :: d -> acc -> a -> b -> c

> instance (Peano d, ZPlus acc (Pos One) acc1) => PeanoDiv' d acc One One acc1 where
>     peanoDiv' d acc One One = acc `zplus` Pos One
> instance (Peano d, Z acc, Peano b) => PeanoDiv' d acc One (Succ b) acc where
>     peanoDiv' d acc One (Succ b) = acc
> instance (Peano d, ZPlus acc (Pos One) acc1, PeanoDiv' d acc1 a d c) => PeanoDiv' d acc (Succ a) One c where
>     peanoDiv' d acc (Succ a) One = peanoDiv' d (acc `zplus` Pos One) a d
> instance (Peano d, Z acc, PeanoDiv' d acc a b c) => PeanoDiv' d acc (Succ a) (Succ b) c where
>     peanoDiv' d acc (Succ a) (Succ b) = peanoDiv' d acc a b

    peanoRem :: Peano -> Peano -> Z
    peanoRem a b = peanoRem' b One a b

> class (PeanoRem' b One a b c) => PeanoRem a b c | a b -> c where
>     peanoRem :: a -> b -> c

> instance (PeanoRem' b One a b c) => PeanoRem a b c where
>     peanoRem a b = peanoRem' b One a b

    peanoRem' :: Peano -> Peano -> Peano -> Peano -> Z
    peanoRem' r acc One One = Zero
    peanoRem' r acc One (Succ b) = Pos acc
    peanoRem' r acc (Succ a) One = peanoRem' r One a r
    peanoRem' r acc (Succ a) (Succ b) = peanoRem' r (Succ acc) a b

> class (Peano r, Peano acc, Peano a, Peano b, Z c) => PeanoRem' r acc a b c | r acc a b -> c where
>     peanoRem' :: r -> acc -> a -> b -> c

> instance (Peano r, Peano acc) => PeanoRem' r acc One One Zero where
>     peanoRem' r acc One One = Zero
> instance (Peano r, Peano acc, Peano b) => PeanoRem' r acc One (Succ b) (Pos acc) where
>     peanoRem' r acc One (Succ b) = Pos acc
> instance (Peano r, Peano acc, PeanoRem' r One a r c) => PeanoRem' r acc (Succ a) One c where
>     peanoRem' r acc (Succ a) One = peanoRem' r One a r
> instance (Peano r, Peano acc, PeanoRem' r (Succ acc) a b c) => PeanoRem' r acc (Succ a) (Succ b) c where
>     peanoRem' r acc (Succ a) (Succ b) = peanoRem' r (Succ acc) a b

    zplus :: Z -> Z -> Z
    zplus Zero b = b
    zplus a Zero = a
    zplus (Neg a) (Neg b) = Neg (peanoPlus a b)
    zplus (Neg a) (Pos b) = peanoMinus b a
    zplus (Pos a) (Neg b) = peanoMinus a b
    zplus (Pos a) (Pos b) = Pos (peanoPlus a b)

> class (Z a, Z b, Z c) => ZPlus a b c | a b -> c where
>     zplus :: a -> b -> c

> instance Z b => ZPlus Zero b b where
>     zplus Zero b = b
> instance Z a => ZPlus a Zero a where
>     zplus a Zero = a
> instance PeanoPlus a b c => ZPlus (Neg a) (Neg b) (Neg c) where
>     zplus (Neg a) (Neg b) = Neg (peanoPlus a b)
> instance PeanoMinus b a c => ZPlus (Neg a) (Pos b) c where
>     zplus (Neg a) (Pos b) = peanoMinus b a
> instance PeanoMinus a b c => ZPlus (Pos a) (Neg b) c where
>     zplus (Pos a) (Neg b) = peanoMinus a b
> instance PeanoPlus a b c => ZPlus (Pos a) (Pos b) (Pos c) where
>     zplus (Pos a) (Pos b) = Pos (peanoPlus a b)

    znegate :: Z -> Z
    znegate Zero = Zero
    znegate (Pos a) = (Neg a)
    znegate (Neg a) = (Pos a)

> class (Z a, Z b) => ZNegate a b | a -> b where
>     znegate :: a -> b

> instance ZNegate Zero Zero where
>     znegate Zero = Zero
> instance Peano a => ZNegate (Pos a) (Neg a) where
>     znegate (Pos a) = (Neg a)
> instance Peano a => ZNegate (Neg a) (Pos a) where
>     znegate (Neg a) = (Pos a)

    zminus :: Z -> Z -> Z
    zminus a b = zplus a (neg b)

> class (Z a, Z b, Z c) => ZMinus a b c | a b -> c where
>     zminus :: a -> b -> c

> instance (Peano b, ZNegate b nb, ZPlus a nb c) => ZMinus a b c where
>     zminus a b = zplus a (znegate b)

    zmultiply :: Z -> Z -> Z
    zmultiply Zero _ = Zero
    zmultiply _ Zero = Zero
    zmultiply (Neg a) (Neg b) = Pos (peanoMultiply a b)
    zmultiply (Neg a) (Pos b) = Neg (peanoMultiply a b)
    zmultiply (Pos a) (Neg b) = Neg (peanoMultiply a b)
    zmultiply (Pos a) (Pos b) = Pos (peanoMultiply a b)

> class (Z a, Z b, Z c) => ZMultiply a b c | a b -> c where
>     zmultiply :: a -> b -> c

> instance Z b => ZMultiply Zero b Zero where
>     zmultiply Zero _ = Zero
> instance Z a => ZMultiply a Zero Zero where
>     zmultiply _ Zero = Zero
> instance PeanoMultiply a b c => ZMultiply (Neg a) (Neg b) (Pos c) where
>     zmultiply (Neg a) (Neg b) = Pos (peanoMultiply a b)
> instance PeanoMultiply a b c => ZMultiply (Neg a) (Pos b) (Neg c) where
>     zmultiply (Neg a) (Pos b) = Neg (peanoMultiply a b)
> instance PeanoMultiply a b c => ZMultiply (Pos a) (Neg b) (Neg c) where
>     zmultiply (Pos a) (Neg b) = Neg (peanoMultiply a b)
> instance PeanoMultiply a b c => ZMultiply (Pos a) (Pos b) (Pos c) where
>     zmultiply (Pos a) (Pos b) = Pos (peanoMultiply a b)

    zdiv :: Z -> Z -> Z
    zdiv Zero (Pos _) = Zero
    zdiv Zero (Neg _) = Zero
    zdiv (Neg a) (Neg b) = peanoDiv a b
    zdiv (Neg a) (Pos b) = znegate (peanoDiv a b)
    zdiv (Pos a) (Neg b) = znegate (peanoDiv a b)
    zdiv (Pos a) (Pos b) = peanoDiv a b
    -- zdiv _ Zero is undefined

> class (Z a, Z b, Z c) => ZDiv a b c | a b -> c where
>     zdiv :: a -> b -> c

> instance Peano b => ZDiv Zero (Pos b) Zero where
>     zdiv Zero (Pos _) = Zero
> instance Peano a => ZDiv Zero (Neg a) Zero where
>     zdiv Zero (Neg _) = Zero
> instance PeanoDiv a b c => ZDiv (Neg a) (Neg b) c where
>     zdiv (Neg a) (Neg b) = peanoDiv a b
> instance (PeanoDiv a b c, ZNegate c c1) => ZDiv (Neg a) (Pos b) c1 where
>     zdiv (Neg a) (Pos b) = znegate (peanoDiv a b)
> instance (PeanoDiv a b c, ZNegate c c1) => ZDiv (Pos a) (Neg b) c1 where
>     zdiv (Pos a) (Neg b) = znegate (peanoDiv a b)
> instance PeanoDiv a b c => ZDiv (Pos a) (Pos b) c where
>     zdiv (Pos a) (Pos b) = peanoDiv a b

    zabs :: Z -> Z
    zabs Zero = Zero
    zabs (Pos a) = Pos a
    zabs (Neg a) = Pos a   

> class (Z a, Z b) => ZAbs a b | a -> b where
>     zabs :: a -> b

> instance ZAbs Zero Zero where
>     zabs Zero = Zero
> instance Peano a => ZAbs (Pos a) (Pos a) where
>     zabs (Pos a) = (Pos a)
> instance Peano a => ZAbs (Neg a) (Pos a) where
>     zabs (Neg a) = (Pos a)

    zgcd :: Z -> Z -> Peano
    zgcd a b = zgcd' (zabs a) (zabs b)

> class (Z a, Z b, Peano c) => ZGcd a b c | a b -> c where
>     zgcd :: a -> b -> c

> instance (ZAbs a b, ZAbs c d, ZGcd' b d e) => ZGcd a c e where
>     zgcd a b = zgcd' (zabs a) (zabs b)

    zgcd' :: Z -> Z -> Peano
    zgcd' (Pos a) Zero = a
    zgcd' Zero (Pos b) = b
    zgcd' (Pos a) (Pos b) = zgcd' (Pos b) (peanoRem a b)
    -- zgcd' Zero Zero is undefined
    -- zgcd' (Neg _) _
    -- zgcd' _ (Neg _) is undefine (zgcd' never called with negative values)

> class (Z a, Z b, Peano c) => ZGcd' a b c | a b -> c where
>     zgcd' :: a -> b -> c

> instance Peano a => ZGcd' (Pos a) Zero a where
>     zgcd' (Pos a) Zero = a
> instance Peano b => ZGcd' Zero (Pos b) b where
>     zgcd' Zero (Pos b) = b
> instance (PeanoRem a b c, ZGcd' (Pos b) c d) => ZGcd' (Pos a) (Pos b) d where
>     zgcd' (Pos a) (Pos b) = zgcd' (Pos b) (peanoRem a b)


At the end we define utility function asInteger which return Haskell Integer
representation for type level integer.

> class AsInteger a where
>     asInteger :: a -> Integer

> instance AsInteger One where
>     asInteger One = 1
> instance (AsInteger a, Peano a) => AsInteger (Succ a) where
>     asInteger (Succ a) = 1 + asInteger a
> instance AsInteger Zero where
>     asInteger Zero = 0
> instance (AsInteger a, Peano a) => AsInteger (Pos a) where
>     asInteger (Pos a) = asInteger a
> instance (AsInteger a, Peano a) => AsInteger (Neg a) where
>     asInteger (Neg a) = - asInteger a


The End.
