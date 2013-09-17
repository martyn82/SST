module Random
where

import Control.Monad
import Data.List
import System.Random
import Techniques
import Week3

import CNF

-- generates a list of random integers
genIntList :: IO [Int]
genIntList = mapM (\ i -> getRandomInt 10) [1..3]

-- determines whether ys is a permutation of xs
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = any (\ perm -> perm == ys) (permutations xs)

-- testable properties of isPermutation
-- a permutation of a list is in fact the ordered set of unique elements of the list.

permutationtests = [([1,2,3], [3,2,1]),
                    ([1,1,1], [1,1,1]),
                    ([1,2,3], [2,3,4]),
                    ([10,20,30,405,1], [10,405,1,20,30])]

pair :: (Monad m) => m [a] -> m [b] -> m ([a], [b])
pair = liftM2 (,)

-- tests the isPermutation function
-- a permutation of x is every ordered list of unique elements of x
--testIsPermutation :: Bool
--testIsPermutation = and (mapM (\ (x, y) -> (isPermutation x y) == (isSameSet x y)) gentests)
--              where isSameSet x y = ((nub (sort x)) == (nub (sort y)))
--                    gentests      = mapM (\ i -> (pair genIntList genIntList)) [1]


-- TODO Define some testable properties for the isPermutation function, and use your random generator for integer lists above to test isPermutation
