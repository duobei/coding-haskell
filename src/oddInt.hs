-- https://www.codewars.com/kata/54da5a58ea159efa38000836

module Codewars.Kata.FindOdd where

-- | Given a list, find the [Int] that appears an 
--   odd number of times. The tests will always
--   provide such a number, and the list will
--   always contain at least one element.
findOdd :: [Int] -> Int
findOdd [x] = x
findOdd (x:xs)
  | even (stat xs x) = x
  | otherwise = findOdd (remove xs x)

stat :: [Int] -> Int -> Int
stat [] e = 0
stat (x:xs) e
  | x == e = 1 + stat xs e
  | otherwise = stat xs e

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x:xs) r = if r == x then remove xs r else x : remove xs r