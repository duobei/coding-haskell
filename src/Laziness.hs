-- https://www.codewars.com/kata/5516b80d891547c9b50007fd

module Laziness where

import Data.List(find)
import Data.Maybe (fromJust)

type Matrix = [[Bool]]

findTrue :: Matrix -> (Int, Int)
findTrue m = fromJust res
  where
    res = find (\(x, y) -> ((m !! x) !! y)) $ [(i, n-i) | n <- [0..], i <- [0..n]]