module Lab5
where

import Data.List

import Week5

-- merge sort
mergeSrt :: Ord a => [a] -> [a]
mergeSrt []     = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

-- merge sort assertive
mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = post1 sorted            -- output must be sorted
            $ assert1 intersectProp -- output must contain the same elements
            $ mergeSrt

-- intersection property of sorted lists
intersectProp :: Ord a => [a] -> [a] -> Bool
intersectProp xs ys = intersect xs ys == xs

-- merge sort using divide and conquer approach
mergeSrt' :: Ord a => [a] -> [a]
mergeSrt' []     = []
mergeSrt' [x]    = [x]
mergeSrt' xs     = merge (mergeSrt' xs1) (mergeSrt' xs2)
            where (xs1, xs2) = split xs

-- split a list into two parts
split :: [a] -> ([a], [a])
split xs = let
        n = (length xs) `div` 2
        in
        (take n xs, drop n xs)

-- assertive version of divide and conquer merge sort
mergeSrt'A :: Ord a => [a] -> [a]
mergeSrt'A = post1 sorted
             $ assert1 intersectProp
             $ mergeSrt'

