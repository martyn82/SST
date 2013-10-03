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

-- Exercise 3 : Time spent 4 hours
-- Formal statement of NRC constraint:
---- Pre and postcondition
---- Sudoku constraints +
---- For all 3x3 blocks with top-left at [(2,2), (2,6), (6,2), (6,6)];
----   For all non-empty values:
----     All values in block are different
----     All values in block are between 1 ... 9

nrcExample1 :: Grid
nrcExample1 = [[0,0,0,3,0,0,0,0,0],
               [0,0,0,7,0,0,3,0,0],
               [2,0,0,0,0,0,0,0,8],
               [0,0,6,0,0,5,0,0,0],
               [0,9,1,6,0,0,0,0,0],
               [3,0,0,0,7,1,2,0,0],
               [0,0,0,0,0,0,0,3,1],
               [0,8,0,0,4,0,0,0,0],
               [0,0,2,0,0,0,0,0,0]]

nrcSolution1 = solveAndShow nrcExample1
