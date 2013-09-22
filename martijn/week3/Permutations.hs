module Permutations
where

import Data.List

import Random

-- determines whether ys is a permutation of xs
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = any (\ perm -> perm == ys) (permutations xs)

-- tests the permutation function by using its property that a list y is a permutation of a list x iff:
-- the sorted lists x' and y' of x and y are equal
-- this test function yields TRUE if the above property holds
testPermutation :: [Int] -> [Int] -> Bool
testPermutation xs ys = (isPermutation xs ys) == ((sort xs) == (sort ys))

-- tests a couple of permutations and prints the test result
testp :: Int -> [[Int]] -> [[Int]] -> IO ()
testp n [] _ = print (show n ++ " tests passed")
testp n _ [] = print (show n ++ " tests passed")
testp n (x:xs) (y:ys) =
        if (testPermutation x y) then do
                print ("pass on: (" ++ show x ++ "," ++ show y ++ "):" ++ (show (isPermutation x y)))
                testp n xs ys
        else error ("failed test on: (" ++ show x ++ "," ++ show y ++ "):" ++ (show (isPermutation x y)))

-- tests permutations by feeding lists of random ints
testPermutations :: Int -> IO ()
testPermutations n = do
        xs <- (mapM (\ i -> genIntList) [1..n])
        ys <- (mapM (\ i -> genIntList) [1..n])
        testp n ys xs

