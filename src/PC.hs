-- https://www.codewars.com/kata/546ed19635a6610900000aa1

{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PC where

import Data.List

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\ac->cd.ac.ba, \bd->dc.bd.ab)

liftISO :: ISO a b -> ISO (a -> a) (b -> b)
liftISO iso = isoFunc iso iso

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 iso = isoFunc iso (liftISO iso)

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero = O
  successor = S

  nat :: a -> (Peano -> a) -> Peano -> a
  nat o _ O     = o
  nat _ f (S n) = f n

  iter o _ O     = o
  iter o f (S n) = f $ iter o f n

  plus n = iter n S

  minus O _         = O
  minus m O         = m
  minus (S m) (S n) = m `minus` n

  mult n = iter O (plus n)

  pow _ O = 1
  pow m (S n) = m * pow m n

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (),
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.

instance Nat [()] where
  zero = []
  successor x = ():x

  nat o _ []     = o
  nat _ f (_:n) = f n

  iter o f = foldl (\x _ -> f x) o

  plus = (++)
  minus = (\\)
  mult = (>>)
  m `pow` n = foldl mult 1 (map (const m) n)

-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching

newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  zero = Scott const
  successor s = Scott $ \_ f -> f s

  nat :: a -> (Scott -> a) -> Scott -> a
  nat o f (Scott n) = n o f

  iter :: a -> (a -> a) -> Scott -> a
  iter o f (Scott n)  = n o (f . iter o f)

  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow
-- Or from induction!

newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
  zero = Church $ \_ x -> x
  successor (Church s) = Church $ \f -> f . (s f)
 
  nat :: a -> (Church -> a) -> Church -> a
  nat o f (Church c) =
    maybe o f $ c (Just . maybe zero successor) Nothing

  iter :: a -> (a -> a) -> Church -> a
  iter o f (Church c) = c f o

  Church m `plus` Church n = Church (\f x -> m f $ n f x)
  m `minus` Church n = n (nat 0 id) m
  Church m `mult` Church n = Church (\f -> m $ n f)
  Church m `pow` Church n = Church $ n m