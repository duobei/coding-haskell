-- https://www.codewars.com/kata/52423db9add6f6fc39000354

module UnlimitedGameOfLife where
import UnlimitedGameOfLife.Preloaded (htmlize)

import Data.List (group, sort)
import Control.Arrow ((&&&), (***))

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration world n | n < 1 = world
                      | otherwise = evolve . life (f world) $ n

f xs = [(i, j) | (i, ys) <- zip [0..] xs, (j, x) <- zip [0..] ys, x>0]

evolve [] = [[]]
evolve alive = [ map (\j -> if (i, j) `elem` alive then 1 else 0) [mj..xj] | i <- [mi..xi]]
    where ((mi, xi), (mj, xj)) = ((minimum &&& maximum) *** (minimum &&& maximum)) . unzip $ alive

life world 0 = world
life world n = life (step world) (n-1)

step :: [(Int, Int)] -> [(Int, Int)]
step cells = [head g | g <- grouped cells, viable g]
  where grouped = group . sort . concatMap neighbors
        neighbors (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]
        viable [_, _, _] = True
        viable [c, _] = c `elem` cells
        viable _ = False