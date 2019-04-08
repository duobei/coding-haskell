-- https://www.codewars.com/kata/52bef5e3588c56132c0003bc

module TreeByLevels where

data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
  } deriving Show

levels :: Maybe (TreeNode a) -> [[a]]
levels Nothing                 = [[]]
levels (Just (TreeNode l r v)) = [v] : zipWith (++) (levels l) (levels r)

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels = concat . levels