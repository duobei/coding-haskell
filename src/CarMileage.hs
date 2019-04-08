-- https://www.codewars.com/kata/52c4dd683bfd3b434c000292

module Awesome.Numbers where

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

incrementing 0 b = False
incrementing 9 0 = True
incrementing a b = a + 1 == b
decrementing a b = a - 1 == b

toDigits :: Integer -> [Integer]
toDigits n
  | n < 10 = [n]
  | otherwise = (toDigits d) ++ [m]
    where (d, m) = n `divMod` 10
    
isRealInteresting x xs =
  let as = toDigits x in
  x > 99 && (
  (all (==0) $ tail as) ||
  as == reverse as ||
  (all id (zipWith incrementing as (tail as))) ||
  (all id (zipWith decrementing as (tail as))) ||
  x `elem` xs)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs
  | isRealInteresting x xs = Yes
  | isRealInteresting (x + 1) xs = Almost
  | isRealInteresting (x + 2) xs = Almost
  | otherwise = No