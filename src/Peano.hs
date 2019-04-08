-- https://www.codewars.com/kata/5779b0f0ec883247b2000117

module Haskell.Codewars.Peano where
import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano
-- Addition
add Zero n = n
add (Succ m) n = add m (Succ n)

-- Subtraction
sub m Zero = m
sub Zero _ = error "negative number"
sub (Succ m) (Succ n) = sub m n

-- Multiplication
mul Zero _ = Zero
mul _ Zero = Zero
mul (Succ m) n = add n (mul m n)

-- Integer division
div _ Zero = error "divide by 0"
div Zero _ = Zero
div m n | compare m n == LT = Zero
        | otherwise = Succ (div (sub m n) n)

even, odd :: Peano -> Bool
-- Even
even Zero = True
even (Succ m) = odd m

-- Odd
odd Zero = False
odd (Succ m) = even m

compare :: Peano -> Peano -> Ordering
-- Compare
compare Zero Zero = EQ
compare Zero _    = LT
compare _ Zero    = GT
compare (Succ m) (Succ n) = compare m n