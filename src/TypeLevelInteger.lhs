> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

Type level integers.

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

    plus :: Z -> Z -> Z
    plus Zero b = b
    plus a Zero = a
    plus (Neg a) (Neg b) = Neg (peanoPlus a b)
    plus (Neg a) (Pos b) = peanoMinus b a
    plus (Pos a) (Neg b) = peanoMinus a b
    plus (Pos a) (Pos b) = Pos (peanoPlus a b)

> class (Z a, Z b, Z c) => Plus a b c | a b -> c where
>     plus :: a -> b -> c

> instance Z b => Plus Zero b b where
>     plus Zero b = b
> instance Z a => Plus a Zero a where
>     plus a Zero = a
> instance PeanoPlus a b c => Plus (Neg a) (Neg b) (Neg c) where
>     plus (Neg a) (Neg b) = Neg (peanoPlus a b)
> instance PeanoMinus b a c => Plus (Neg a) (Pos b) c where
>     plus (Neg a) (Pos b) = peanoMinus b a
> instance PeanoMinus a b c => Plus (Pos a) (Neg b) c where
>     plus (Pos a) (Neg b) = peanoMinus a b
> instance PeanoPlus a b c => Plus (Pos a) (Pos b) (Pos c) where
>     plus (Pos a) (Pos b) = Pos (peanoPlus a b)

    negate :: Z -> Z
    negate Zero = Zero
    negate (Pos a) = (Neg a)
    negate (Neg a) = (Pos a)

> class (Z a, Z b) => Negate a b | a -> b where
>     neg :: a -> b

> instance Negate Zero Zero where
>     neg Zero = Zero
> instance Peano a => Negate (Pos a) (Neg a) where
>     neg (Pos a) = (Neg a)
> instance Peano a => Negate (Neg a) (Pos a) where
>     neg (Neg a) = (Pos a)

    minus :: Z -> Z -> Z
    minus a Zero = a
    minus a b = plus a (neg b)

> class (Z a, Z b, Z c) => Minus a b c | a b -> c where
>     minus :: a -> b -> c

> instance Z a => Minus a Zero a where
>     minus a Zero = a
> instance (Peano b, Negate b nb, Plus a nb c) => Minus a b c where
>     minus a b = plus a (neg b)

    multiply :: Z -> Z -> Z
    multiply Zero _ = Zero
    multiply _ Zero = Zero
    multiply (Neg a) (Neg b) = Pos (peanoMultiply a b)
    multiply (Neg a) (Pos b) = Neg (peanoMultiply a b)
    multiply (Pos a) (Neg b) = Neg (peanoMultiply a b)
    multiply (Pos a) (Pos b) = Pos (peanoMultiply a b)

> class (Z a, Z b, Z c) => Multiply a b c | a b -> c where
>     multiply :: a -> b -> c

> instance Z b => Multiply Zero b Zero where
>     multiply Zero _ = Zero
> instance Z a => Multiply a Zero Zero where
>     multiply _ Zero = Zero
> instance PeanoMultiply a b c => Multiply (Neg a) (Neg b) (Pos c) where
>     multiply (Neg a) (Neg b) = Pos (peanoMultiply a b)
> instance PeanoMultiply a b c => Multiply (Neg a) (Pos b) (Neg c) where
>     multiply (Neg a) (Pos b) = Neg (peanoMultiply a b)
> instance PeanoMultiply a b c => Multiply (Pos a) (Neg b) (Neg c) where
>     multiply (Pos a) (Neg b) = Neg (peanoMultiply a b)
> instance PeanoMultiply a b c => Multiply (Pos a) (Pos b) (Pos c) where
>     multiply (Pos a) (Pos b) = Pos (peanoMultiply a b)


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


Some test code below...

Num and Show class instances for function level integers code test.

    instance Num Peano where
        a + b = peanoPlus a b
        fromInteger n | n > 1     = Succ (fromInteger (n-1))
                      | n == 1    = One 
                      | otherwise = error "peano numbers starts from One"

    instance Num Z where
        a + b = plus a b
        negate a = minus Zero a
        fromInteger n | n > 0  = Pos (fromInteger n)
                      | n < 0  = Neg (fromInteger (-n))
                      | otherwise = Zero

    instance Show Peano where
        show = show . toInt 0
            where toInt acc One = acc + 1
                  toInt acc (Succ a) = toInt (acc + 1) a

    instance Show Z where
        show Zero = "0"
        show (Pos n) = show n
        show (Neg n) = '-' : show n
