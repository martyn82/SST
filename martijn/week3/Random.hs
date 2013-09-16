module Random
where

import Data.List
import System.Random
import Techniques
import Week3

import CNF

import System.IO.Unsafe

-- generates a list of random integers
genIntList :: [Int]
genIntList = genIntList' 0 []
       -- the following works, but its wrong... *unsafe* is unintentionally changing the return type from IO [Int] to [Int]
       where genIntList' n ls | n < 3     = genIntList' (succ n) ((unsafePerformIO $ getRandomInt 10) : ls)
                              | otherwise = ls

-- determines whether ys is a permutation of xs
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = any (\ perm -> perm == ys) (permutations xs)

-- TODO Define some testable properties for the isPermutation function, and use your random generator for integer lists above to test isPermutation
-- Why not done so? The IO is causing problems.


