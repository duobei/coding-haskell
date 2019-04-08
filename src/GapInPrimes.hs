-- https://www.codewars.com/kata/561e9c843a2ef5a40c0000a4

module Codewars.G964.GapInPrimes where

import Data.List (find)
import Control.Arrow ((&&&))

gap :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
gap g m n = find (\(a, b) -> b-a == g) primeTuples
  where
    primeTuples = uncurry zip . (id &&& drop 1) . filter isPrime $ [m..n]

isPrime n = go 2
  where
    go d
      | d*d > n        = True
      | n `rem` d == 0 = False
      | otherwise      = go (d+1)