-- https://www.codewars.com/kata/542332c8c2cf7ccbbb000005

module BitTwiddles where
import Prelude hiding ((*), (/), (^), (&&), (||), fromIntegral, fromInteger, mod, div, quot, rem, quotRem, divMod, (>), (<), compare, Ordering(..), abs, even, odd)
import Data.Bits hiding (popCount)
import Data.Int (Int32)

isEven :: Bits a => a -> Bool
isEven = not . isOdd

isOdd  :: Bits a => a -> Bool
isOdd  = flip testBit 0

halfAndFloor  :: Bits a => a -> a
halfAndFloor n = shiftR n 1

isPowerOfTwo :: (Num a, Bits a) => a -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo n = n .&. (n-1) == 0

nthPowerOfTwo :: (Num a, Bits a) => Int -> a
nthPowerOfTwo = shiftL 1

abs :: Int32 -> Int32
abs v = (v + mask) `xor` mask where mask = shiftR v 31