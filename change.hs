module Main where

import Data.List as L
import Data.Array
import Debug.Trace
import qualified Data.Set as Set

combos :: [Int] -> Int -> Int
combos denominations target = snd $ arr ! (L.length denominations, target)
  where
    arr = listArray ((0,0), ((L.length denominations), target)) [ ((denominationCount, currentTarget), totalCombos denominationCount currentTarget) | denominationCount <- [0..(L.length denominations)], currentTarget <- [0..target]]
    totalCombos 0 _ = 1
    totalCombos _ 0 = 1
    totalCombos denominationCount currentTarget = sum [ snd (arr ! (denominationCount - 1, remaining)) | remaining <- diffList currentTarget (take denominationCount denominations)]
    

diffList :: Int -> [Int] -> [Int]
diffList start denominations = [ start - value | value <- denominations, value <= start]

comboPatterns :: [Int] -> Int -> Set.Set [Int]
comboPatterns denominations target = arr ! target
  where
    arr = listArray (0, target) ((Set.singleton [] :: Set.Set [Int]) : L.map m [1..target])
    m total = Set.unions [ Set.map (sort . (:) value) (arr ! (total - value)) | value <- denominations, value <= total]
      
main = do
  denominations <- getLine
  target <- getLine
  putStrLn $ show $ combos (read ("[" ++ denominations ++ "]")) (read target)

  

