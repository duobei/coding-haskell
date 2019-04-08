-- https://www.codewars.com/kata/56662e268c0797cece0000bb

module Codewars.Fibonacci where

sumFibs :: Int -> Integer
sumFibs n = sum . filter even $ take n fibs

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)