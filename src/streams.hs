-- https://www.codewars.com/kata/5512ec4bbe2074421d00028c

module Stream where

import Control.Arrow
import Control.Applicative

import Stream.Internal


-- | Get the first element of a stream.
headS :: Stream a -> a
headS (x :> _)= x

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (_ :> xs)= xs


-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS x = x :> repeatS x

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS xs = foldr (:>) (cycleS xs) xs

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS x = x :> fromS (x + 1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = x :> fromStepS (x + s) s

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x (foldrS f xs)

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x :> xs) = if p x then x :> filterS p xs else filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS i s | i <= 0 = []
takeS i (x :> xs) = x : takeS (i - 1) xs

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i s | i <= 0 = s
dropS i (x :> xs) = dropS (i - 1) xs

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = (takeS i s, dropS i s)

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure = repeatS

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = 0 :> 1 :> zipWithS (+) fibS (tailS fibS)

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = sieve [2..]

sieve (p:xs) = p :> sieve [x | x <- xs, x `mod` p > 0]