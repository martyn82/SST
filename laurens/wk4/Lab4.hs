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
-- Set union: A union B = x in A or x in B
-- unionSet :: (Ord a) => Set a -> Set a -> Set a
-- unionSet (Set [])     set2  = set2
-- unionSet (Set (x:xs)) set2  = insertSet x (unionSet (Set xs) set2)

-- Set intersection: A intersect B = x in A and x in B
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) _     = Set []
intersectSet (Set (x:xs)) s | inSet x s  = insertSet x (intersectSet (Set xs) s)
                            | otherwise  = intersectSet (Set xs) s

-- Set difference: A - B = x in A and x not in B
differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet s (Set [])     = s
differenceSet s (Set (y:ys)) | inSet y s = differenceSet (deleteSet y s) (Set ys)
                             | otherwise = differenceSet s (Set ys)
