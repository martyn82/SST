module Permutations where

import Data.List
import Techniques
import Random

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (x:xs) ys | elem x ys = (x : intersect' xs (delete x ys))
                     | otherwise = intersect' xs ys

-- Exercise 4:
-- determines whether ys is a permutation of xs
-- precondition: list is finite.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | length xs /= length ys = False
                    | otherwise = isPermutation' xs ys

-- precondition: lists are of equal length.
isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation' [] [] = True
isPermutation' (x:xs) ys | x `elem` ys = isPermutation' xs (delete x ys)
                         | otherwise = False

-- Exercise 5:
-- A list is a permutation of another list, if there is no difference between the two lists
testPermutationListDifference :: Eq a => [a] -> [a] -> Bool
testPermutationListDifference xs ys = (xs \\ ys) == [] && (ys \\ xs) == []

-- Previous also implies that the intersection of both lists, always yields the entire list.
testPermutationListIntersection :: Eq a => [a] -> [a] -> Bool
testPermutationListIntersection xs ys = (xs `intersect'` ys) == xs && (ys `intersect'` xs) == ys

-- If list A is a permutation of list B, than the sorted lists A' and B' are equal.
testPermutationSortedListEquality :: Ord a => [a] -> [a] -> Bool
testPermutationSortedListEquality xs ys = (sort xs) == (sort ys)

-- tests a couple of permutations and prints the test result
testp :: Int -> ([Int] -> [Int] -> Bool) -> [[Int]] -> [[Int]] -> IO ()
testp n p [] _ = print (show n ++ " tests passed")
testp n p _ [] = print (show n ++ " tests passed")
testp n p (x:xs) (y:ys) =
          if ((isPermutation x y) == (p x y)) then do
                  print ("pass on: (" ++ show x ++ "," ++ show y ++ "):" ++ (show (isPermutation x y)))
                  testp n p xs ys
          else error ("failed test on: (" ++ show x ++ "," ++ show y ++ "):" ++ (show (isPermutation x y)))

-- tests permutations by feeding lists of random ints and a property to the isPermutation method.
-- testPermutations (# tests) (# max list length) (property method)
testPermutations :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO ()
testPermutations n l p = do
        l1 <- getRandomInt l
        l2 <- getRandomInt l
        xs <- (mapM (\ i -> genIntList' l1) [1..n])
        ys <- (mapM (\ i -> genIntList' l2) [1..n])
        testp n p ys xs

