-- https://www.codewars.com/kata/56a5d994ac971f1ac500003e

module Codewars.G964.Longestconsec where


longestConsec :: [String] -> Int -> String
longestConsec strarr k
  | length strarr == 0 || k > length strarr || k <= 0 = ""
  | otherwise = segments strarr k 0 ""

segments :: [String] -> Int -> Int -> String -> String
segments strarr k len temp = if length strarr < k 
                             then
                               temp
                             else
                               if l > len
                               then segments t k l seg
                               else segments t k len temp
                               where
                                 seg = concat $ take k strarr
                                 l   = length seg
                                 t   = tail strarr