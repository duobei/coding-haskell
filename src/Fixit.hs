-- https://www.codewars.com/kata/5443dd2d7fc4478154000ac6

module Fixit where
import Prelude hiding (reverse, foldr)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f [] = []
reverse' f (x:xs) = f xs ++ [x]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' f g z []     = z
foldr' f g z (x:xs) = g x (f g z xs)