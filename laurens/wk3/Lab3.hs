module Lab3 where

import Data.List
import Techniques
import Week3

-- Exercise 3 : Time spent 2 hours (hard to understand how to work with IO monad)
-- generates a list of 10 elements with random values in the range of 0-10.
genIntList :: IO [Int]
genIntList = getRandomInts 10 10

-- getRandomInts maxValue, amount, list of random ints.
getRandomInts :: Int -> Int -> IO [Int]
getRandomInts _ 0 = return []
getRandomInts m n = do x <- getRandomInt m
                       xs <- getRandomInts m (n-1)
                       return (x:xs)

-- Exercise 4 : Time spent 1 hour (had to find a somewhat efficient way to check this, because this method blows up because of the amount of permutations (n!))
-- precondition: list is finite.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | length xs /= length ys = False
                    | otherwise = isPermutation' xs ys

-- precondition: lists are of equal length.
isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation' [] [] = True
isPermutation' [] _ = False
isPermutation' (x:xs) ys | x `elem` ys = isPermutation' xs (delete x ys)
                         | otherwise = False
