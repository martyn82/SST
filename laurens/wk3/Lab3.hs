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

-- Exercise 5 : Time spent 3 hours (spent some time to identify some properties, spent a lot time on implementing the test methods)
-- Some list is a permutation of another list if the lists are of equal length
testPermutationLengthEquality :: Eq a => [a] -> [a] -> Bool
testPermutationLengthEquality xs ys = length xs == length ys

-- A list is a permutation of another list, if there is no difference between the two sets
testPermutationListDifference :: Eq a => [a] -> [a] -> Bool
testPermutationListDifference xs ys = (xs \\ ys) == [] && (ys \\ xs) == []

-- Previous also implies that the intersection of both lists, always yield the entire list.
testPermutationListIntersection :: Eq a => [a] -> [a] -> Bool
testPermutationListIntersection xs ys = (xs `intersect` ys) == xs && (ys `intersect` xs) == ys

-- If it is a permutation on some list, it also is a permutation on the sets of these lists.
testPermutationOnUniqueList :: Eq a => [a] -> [a] -> Bool
testPermutationOnUniqueList xs ys = isPermutation (nub xs) (nub ys)

-- test our permutation properties on n random lists.
-- testPermutations (# lists) (# maxValue) (property method) -> ([Result of IsPermuation, Result of Property], [Inputs])
testPermutations :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO [([Bool], [[Int]])]
testPermutations 0 _ _ = return []
testPermutations n m prop = do x <- testPermutations' m prop
                               xs <- testPermutations (n-1) m prop
                               return (x:xs)

-- testPermutations' (# maxValue) (property method) -> ([Result of IsPermuation, Result of Property], [Inputs])
testPermutations' :: Int -> ([Int] -> [Int] -> Bool) -> IO ([Bool], [[Int]])
testPermutations' m prop = do l1 <- getRandomInt m
                              l2 <- getRandomInt m
                              xs <- getRandomInts m l1
                              ys <- getRandomInts m l2
                              -- print ("Testcase: " ++ show xs ++ ", " ++ show ys)
                              let b1 = isPermutation xs ys
                              -- print ("IsPermutation: " ++ show b1)
                              let b2 = prop xs ys
                              -- print ("Property: " ++ show b2)
                              return ([b1, b2], [xs, ys])

-- testAllPermuationProperties (# tests) (# maxvalue)
testAllPermutationProperties n m = do p1 <- testPermutations n m testPermutationLengthEquality
                                      p2 <- testPermutations n m testPermutationListDifference
                                      p3 <- testPermutations n m testPermutationListIntersection
                                      p4 <- testPermutations n m testPermutationOnUniqueList
                                      -- select those results that yielded True on IsPermutation or Property or Both.
                                      let p1' = filter (\x -> any (\y -> y) (fst x)) p1
                                      let p2' = filter (\x -> any (\y -> y) (fst x)) p2
                                      let p3' = filter (\x -> any (\y -> y) (fst x)) p3
                                      let p4' = filter (\x -> any (\y -> y) (fst x)) p4
                                      return (p1' ++ p2' ++ p3' ++ p4')
