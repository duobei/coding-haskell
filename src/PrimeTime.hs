-- https://www.codewars.com/kata/521ef596c106a935c0000519

module PrimeTime where

prime :: Int -> [Int]
prime n = takeWhile (<= n) . sieve $ [2..]
    where sieve (p:xs) = p : sieve [x | x <- xs , x `mod` p > 0]