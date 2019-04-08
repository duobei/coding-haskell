-- https://www.codewars.com/kata/5296bc77afba8baa690002d7

module Sudoku where

type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Int
type Choices = [Digit]

digits = [1..9]
blank = (== 0)

choices :: Grid -> Matrix Choices
choices = map (map choice)
choice d = if blank d then digits else [d]

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)
          
nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/= x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove fixed) row
               where fixed = [d | [d] <- row]

remove :: Choices -> Choices -> Choices
remove ds [x] = [x]
remove ds xs = filter (`notElem` ds) xs

expand :: Matrix Choices -> [Matrix Choices]
expand rows = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
               where
                 (rows1, row:rows2) = break (any smallest) rows
                 (row1, cs:row2) = break smallest row
                 smallest cs = length cs == n
                 n = minimum (counts rows)
                 
counts = filter (/= 1) . map length . concat

safe :: Matrix Choices -> Bool
safe m = all ok (rows m) &&
         all ok (cols m) &&
         all ok (boxs m)
         where ok row = nodups [x | [x] <- row]

complete :: Matrix Choices -> Bool
complete = all (all single)
           where
             single [_] = True
             single _ = False

extract :: Matrix Choices -> Grid
extract = map (map head)

pruneBy f = f . map pruneRow . f
prune = pruneBy boxs . pruneBy cols . pruneBy rows

search cm
  | not (safe pm) = []
  | complete pm = [extract pm]
  | otherwise = concat (map search (expand pm))
  where pm = prune cm

sudoku :: Grid -> Grid
sudoku = head . search . choices