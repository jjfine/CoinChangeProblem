module Main where

import Data.List

cc_list :: [Int] -> [Int]
cc_list denominations = map (countCombinations' countCombinations denominations) [0..]

countCombinations :: [Int] -> Int -> Int
countCombinations denominations target = (cc_list denominations) !! target

countCombinations' :: ([Int] -> Int -> Int) -> [Int] -> Int -> Int
countCombinations' _ [] _ = 0 
countCombinations' _ _ 0 = 0
countCombinations' cc (x:xs) target 
    | x > target = cc xs target
    | x == target = 1
    | x < target = ((cc (x:xs) (target - x)) + (cc  xs target))


main :: IO ()
main = do
    denominations <- getLine
    target <- getLine
    print $ countCombinations (read ( "[" ++ denominations ++ "]" ))  (read target) 
