-- https://www.codewars.com/kata/56c04261c3fcf33f2d000534

module Codewars.G964.Doubles where

sumN :: Double -> Double -> Double -> Double -> Double -> Double
sumN maxn k n vk uk
  | n > maxn = uk
  | otherwise = sumN maxn k (n + 1) nvk nuk
      where
        item = (n / (n + 1)) ** (2.0 * k)
        nvk = vk * item
        nuk = uk + nvk
        
sumK :: Double -> Double -> Double -> Double -> Double -> Double
sumK maxk maxn k n result
  | k > maxk = result
  | otherwise = sumK maxk maxn (k + 1) n (result + itemV)
      where
        factor = 1 / (k * 2.0 ** (2.0 * k))
        itemV = sumN maxn k n factor factor

doubles maxk maxn = sumK (fromIntegral maxk) (fromIntegral maxn) 1 2 0