module Main where

import Data.List
import Data.Array
import Debug.Trace
import qualified Data.Set as Set

combos :: [Int] -> Int -> Int
combos denominations target = arr ! target
  where
    arr = listArray (0, target) (1: Data.List.map m [1..target])
    m total = sum [  arr ! (total - value) | value <- denominations, value <= total]

comboPatterns :: [Int] -> Int -> Set.Set [Int]
comboPatterns denominations target = arr ! target
  where
    arr = listArray (0, target) ((Set.singleton [] :: Set.Set [Int]) : Data.List.map m [1..target])
    m total = Set.unions [ Set.map (sort . (:) value) (arr ! (total - value)) | value <- denominations, value <= total]
      
main = do
  denominations <- getLine
  target <- getLine
  print $ Set.size $ comboPatterns (read ("[" ++ denominations ++ "]")) (read target)

