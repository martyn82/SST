module Lab3 where

import Techniques
import Week3

-- Exercise 3 : Time spent (2 hours, hard to understand how to work with IO monad)
-- generates a list of 10 elements with random values in the range of 0-10.
genIntList :: IO [Int]
genIntList = getRandomInts 10 10

-- getRandomInts maxValue, amount, list of random ints.
getRandomInts :: Int -> Int -> IO [Int]
getRandomInts _ 0 = return []
getRandomInts m n = do x <- getRandomInt m
                       xs <- getRandomInts m (n-1)
                       return (x:xs)
