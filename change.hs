module Main where

import Data.List
import Data.Set
    
data Tree a = Root [Tree a] | Node a [Tree a] deriving (Show, Read, Eq) 

infiniteTree :: [Int] -> Tree Int
infiniteTree values = Root [treeBranch values nextBranchValue | nextBranchValue <- values]

treeBranch :: [Int] -> Int -> Tree Int
treeBranch values branchValue = Node branchValue [treeBranch values nextBranchValue | nextBranchValue <- values]


findTotal ::  Int -> [Int] -> Tree Int -> [[Int]]
findTotal targetValue _ (Root branches) = concatMap (findTotal targetValue []) branches
findTotal targetValue pattern (Node branchValue branches)
    | branchValue + (sum pattern) == targetValue = [ pattern ++ [branchValue] ]
    | branchValue + (sum pattern) > targetValue = []
    | branchValue + (sum pattern) < targetValue = concatMap (findTotal targetValue (pattern++[branchValue])) branches

countDistinct :: [[Int]] -> Int
countDistinct = size . fromList . Data.List.map sort

main :: IO ()
main = do
    print $ countDistinct . findTotal 4 [] $ infiniteTree [1,2,3]
    
