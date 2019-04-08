-- https://www.codewars.com/kata/54c2fc0552791928c9000517
module Codewars.Kata.Summing where

sum' :: Integer -> Integer
sum' n = div (n*(n+1)) 2

f :: Integer -> Integer -> Integer
f n m = (sum' (m-1)) * (fst tup) + sum' (snd tup) where tup = divMod n m