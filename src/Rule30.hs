-- https://www.codewars.com/kata/5581e52ac76ffdea700000c1

module Codewars.Kata.Rule30 where

trans :: [Int] -> Int
trans [0, 0, 0] = 0
trans [0, 0, 1] = 1
trans [0, 1, 0] = 1
trans [0, 1, 1] = 1
trans [1, 0, 0] = 1
trans [1, 0, 1] = 0
trans [1, 1, 0] = 0
trans [1, 1, 1] = 0

split :: [Int] -> [Int]
split cells
  | length cells < 3 = []
  | otherwise = trans (take 3 cells) : split (drop 1 cells)
  
turn :: Int -> Int
turn 1 = 1
turn _ = 0

rule30' :: [Int] -> Int -> [Int]
rule30' cells n
  | n < 1 = cells
  | otherwise =  rule30' newCells (n-1)
                 where
                    newCells = split $ [0, 0] ++ cells ++ [0, 0]
                    
rule30 :: [Int] -> Int -> [Int]
rule30 xs = rule30' (map turn xs)