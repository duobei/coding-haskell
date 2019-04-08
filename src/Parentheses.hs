-- https://www.codewars.com/kata/52774a314c2333f0a7000688

module Codewars.Parentheses where

import Data.List (foldl')

validParentheses :: String -> Bool
validParentheses xs = null $ foldl' op [] xs
          where
            op ('(':xs) ')' = xs
            op xs x = x:xs