module Set
where

import Data.List (intersect, (\\))
import System.Random

import SetOrd

listsize = 5
minrand  = 0
maxrand  = 10

-- generates a random Int between n and m (inclusive)
getRandomInt :: Int -> Int -> IO Int
getRandomInt n m = getStdRandom (randomR (n, m))

-- generates a list of random Ints
genIntList :: IO [Int]
genIntList = sequence $ (take listsize stream)
           where stream = (getRandomInt minrand maxrand) : stream

-- generates a random set
genRandomSet :: IO (Set Int)
genRandomSet = do
            m <- getRandomInt 0 1
            case m of
                0 -> do return emptySet
                1 -> do
                    ls <- genIntList
                    return (list2set ls)


-- unionSet already defined in SetOrd module

-- intersectSet gives the intersection of two Sets
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = list2set (intersect xs ys)

-- diffSet gives the non-associative difference between two sets
diffSet :: (Ord a) => Set a -> Set a -> Set a
diffSet (Set xs) (Set ys) = list2set (xs \\ ys)

