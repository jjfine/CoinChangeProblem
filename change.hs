-- | Main entry point to the application.
module Main where

data Tree a = EmptyTree | Node a [Tree a] deriving (Show, Read, Eq) 

nullTree :: [Int] -> Int -> Tree Int
nullTree values total = Node total [nullTree values (total+branch) | branch <- values]


findTotal ::  Int -> Tree Int -> Int
findTotal targetValue (Node total branches)
    | total == targetValue = 1
    | total > targetValue = 0
    | total < targetValue = sum $ map (findTotal targetValue) branches
    
-- | build infinite tree where each branch B2,B2...BD from each node N is weighted for each denomination D

-- | calculate number of combinations by doing depth first search until sum of branch weights equals total T

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
    print $ findTotal 4 $ nullTree [1,2,3] 0 
    
