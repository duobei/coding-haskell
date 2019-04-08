-- https://www.codewars.com/kata/5701e43f86306a615c001868
module Haskell.Codewars.CreditCardIssuer where

getIssuer :: Int -> String
getIssuer number
  | head num == '4' && (elem (length num) [13, 16]) = "VISA"
  | take 4 num == "6011" && length num == 16 = "Discover"
  | (take 2 num == "34" || take 2 num == "37") && length num == 15 = "AMEX"
  | num !! 0 == '5' && elem (num !! 1) ['1'..'5'] && length num == 16 = "Mastercard"
  | otherwise = "Unknown"
  where num = show number