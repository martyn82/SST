module Lab4 where

import Week4
import SetOrd

import System.Random

-- Exercise 2: Time spent 1 hour
-- genIntList from wk 3
getRandomInt :: IO Int
getRandomInt = getStdRandom random

genIntList :: Int -> IO [Int]
genIntList n = sequence $ (take n stream)
                   where stream = getRandomInt : stream

-- getRandomInt' ((#minValue, #maxValue))
getRandomInt' :: (Int, Int) -> IO Int
getRandomInt' n = getStdRandom (randomR n)

-- genIntList' (#lists) ((#minValue, #maxValue))
genIntList' :: Int -> (Int, Int) -> IO [Int]
genIntList' n m = sequence $ (take n stream)
                      where stream = (getRandomInt' m) : stream

-- gets a random set with max 10 elements (depends on the randomness of genIntList)
getRandomSet :: IO (Set Int)
getRandomSet = do xs <- genIntList 10
                  return (list2set xs)

-- getRandomSet' (#max length of set) ((#minValue, #maxValue))
getRandomSet' :: Int -> (Int, Int) -> IO (Set Int)
getRandomSet' ml mv = do xs <- genIntList' ml mv
                         return (list2set xs)

-- Exercise 3:
