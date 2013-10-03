module Lab5

where
import Data.List
import Week5

-- Exercise 1 : Time spent 1 hour
mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

-- Elements of the orignal list have to be in the sorted list.
mergeSrtElementsProp :: Ord a => [a] -> [a] -> Bool
mergeSrtElementsProp xs ys = xs \\ ys == [] && ys \\ xs == []

-- Elements in the outpur are always sorted
mergeSrtSortedProp :: Ord a => [a] -> [a] -> Bool
mergeSrtSortedProp _ ys = sorted ys

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 mergeSrtSortedProp 
              $ assert1 mergeSrtElementsProp mergeSrt

-- Exercise 2 : Time spent 1 hour
split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2
           in (take n xs, drop n xs)

mergeSrt' :: Ord a => [a] -> [a]
mergeSrt' []  = []
mergeSrt' [x] = [x]
mergeSrt' xs  = let (x,y) = split xs
                in merge (mergeSrtA' x) (mergeSrtA' y) -- Auto testing of in between steps.

mergeSrtA' :: Ord a => [a] -> [a]
mergeSrtA' = assert1 mergeSrtSortedProp 
              $ assert1 mergeSrtElementsProp mergeSrt'
