-- https://www.codewars.com/kata/53d40c1e2f13e331fc000c26

module Fibonacci where

calc n =
  case n of
    0 -> (0, 1)
    1 -> (1, 1)
    _ -> if even n then (p, q) else (q, p + q)
      where
        (a, b) = calc(n `div` 2)
        p = a * (2 * b - a)
        q = b * b + a * a

fib :: Integer -> Integer
fib n
  | n >= 0    = fst . calc $ n
  | even n    = negate . fst . calc $ -n
  | otherwise = fst . calc $ -n