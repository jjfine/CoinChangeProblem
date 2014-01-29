module Main where

import Data.List
import Data.Array
import Debug.Trace
import Data.Set

combos :: [Int] -> Int -> Int
combos denominations target = arr ! target
  where
      arr = listArray (0, target) (1: Data.List.map m [1..target])
      m total = sum [  arr ! (total - value) | value <- denominations, value <= total]

comboPatterns :: [Int] -> Int -> [[Int]]
comboPatterns denominations target = arr ! target
  where
      arr = listArray (0, target) (([[]] :: [[Int]]) : Data.List.map m [1..target])
      m total = concat [ Data.List.map ((:) value) (arr ! (total - value)) | value <- denominations, value <= total]
      
main = do
    denominations <- getLine
    target <- getLine
    print $ Data.Set.size . fromList $ Data.List.map sort $ comboPatterns (read ( "[" ++ denominations ++ "]" ))  (read target)
