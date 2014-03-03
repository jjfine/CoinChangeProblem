module Main where

import Data.List as L
import Data.Array
import Debug.Trace
import qualified Data.Set as Set

combos :: [Int] -> Int -> Int
combos denominations target = snd $ arr ! (L.length denominations, target)
  where
    arr = listArray ((0,0), ((L.length denominations), target)) [ ((denominationCount, currentTarget), totalCombos denominationCount currentTarget) | denominationCount <- [0..(L.length denominations)], currentTarget <- [0..target]]
    totalCombos 0 0 = 1
    totalCombos _ 0 = 1
    totalCombos 0 _ = 0
    totalCombos denominationCount currentTarget = (retrieveCombos (denominationCount - 1) currentTarget) + (retrieveCombos denominationCount (currentTarget - (denominations !! (denominationCount - 1))))
    retrieveCombos denominationCount remaining  
            | remaining < 0 = 0
            | remaining >= 0 = snd (arr ! (denominationCount, remaining))

main = do
  denominations <- getLine
  target <- getLine
  putStrLn $ show $ combos (read ("[" ++ denominations ++ "]")) (read target)

  

