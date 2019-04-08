-- https://www.codewars.com/kata/54f5f22a00ecc4184c000034

module Codewars.Kata.ExpAsFract where

import Data.Ratio (numerator, denominator, approxRational)

expand :: Double -> Int -> (Integer, Integer)
expand x digit = (numerator first, denominator first)
  where
    first = head . dropWhile ((<digit) . length . show . numerator) . expandList $ x


expandList x = scanl (+) 0 . zipWith (/) (iterate (*y) 1) $ scanl (*) 1 [1..]
  where
    y = approxRational x 1e-6