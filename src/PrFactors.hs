-- https://www.codewars.com/kata/54d512e62a5e54c96200019e

module Codewars.Kata.PrFactors where

import Data.List (group)
import Control.Arrow ((&&&))

prime_factors :: Integer -> String  
prime_factors = prints . map (head &&& length) . group . factors

isPrime n | n < 2 = False
isPrime n = all (\p -> n `mod` p /= 0) . takeWhile ((<= n) . (^ 2)) $ primes

primes = 2 : filter isPrime [3..]

factors n = iter n primes where
    iter n (p:_) | n < p^2 = [n | n > 1]
    iter n ps@(p:ps') =
        let (d, r) = n `divMod` p
        in if r == 0 then p : iter d ps else iter n ps'

prints ts =
  case ts of
    []   -> ""
    x:xs -> s (fst x) (snd x) ++ prints xs
  where
    s prime count = "(" ++ show prime ++ (if count > 1 then "**" ++ show count else "" ) ++ ")"
