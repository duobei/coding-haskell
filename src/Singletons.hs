-- https://www.codewars.com/kata/54750ed320c64c64e20002e2

{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero            = False
type instance Zero :< Succ n       = True
type instance (Succ m) :< (Succ n) = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero m     = m
type instance Add (Succ n) m = Succ (Add n m)

type family (Subtract (a :: Nat) (b :: Nat)) :: Nat
type instance Subtract Zero m = Zero
type instance Subtract m Zero = m
type instance Subtract (Succ n) (Succ m) = Subtract n m

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance Min Zero m     = Zero
type instance Min m Zero     = Zero
type instance Min (Succ n) (Succ m) = Succ (Min n m)


map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons a _ ) = a
index (SSucc sm') (VCons _ as) = index sm' as

replicate :: s -> SNat a -> Vec s a
replicate _ SZero     = VNil
replicate a (SSucc n) = VCons a (replicate a n)

-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)
zipWith _ VNil VNil                 = VNil

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil  ++ v2       = v2
(VCons h t) ++ v2 = VCons h (t ++ v2) 

-- The semantics should match that of take for normal lists.
take :: SNat a -> Vec v m -> Vec v (Min a m)
take SZero _  = VNil
take _ VNil   = VNil
take (SSucc m) (VCons x xs) = VCons x (take m xs)

-- The semantics should match that of drop for normal lists.
drop :: SNat a -> Vec v m -> Vec v (Subtract m a)
drop SZero v = v
drop _ VNil  = VNil
drop (SSucc m) (VCons x xs) = drop m xs

head :: Vec a m -> a
head (VCons x _) = x

tail :: Vec a (Succ m) -> Vec a m
tail (VCons _ xs)= xs