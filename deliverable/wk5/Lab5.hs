module Lab5
where

import Data.List (intersect)

import Week5 (assert1, post1, merge, sorted)

import GenericSudoku -- sudokus of both normal and NRC types
import RandomSudoku  -- random sudoku problem generator for both normal and NRC

-- Exercise 1
-- Find a suitable assertion, and write an assertive version of this.
-- Deliverables: Assertion, Haskell program that uses this assertion, indication of time
-- spent

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

-- in each step of merge sort, the intersection of a b is identity of a
intersectYieldsItself :: (Ord a) => [a] -> [a] -> Bool
intersectYieldsItself a b = intersect a b == a

-- assertive version of mergeSrt
mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = post1   sorted 
          $ assert1 intersectYieldsItself
          $ mergeSrt

-- Exercise 2
-- Another approach to merge sort is to start by splitting the list to be sorted in equal
-- parts, recursively sort the parts, next merge.
-- Implement this, using the following split function.

-- Next, find a suitable assertion, and write an assertive version.
-- Deliverables: Haskell program, assertion, assertive version of Haskell program that
-- uses this assertion, indication of time spent.

-- split a list into (near) halfs
split :: [a] -> ([a],[a])
split xs = let
            n = (length xs) `div` 2
            in
            (take n xs, drop n xs)

-- merge sort using divide and conquer approach
mergeSrt' :: (Ord a) => [a] -> [a]
mergeSrt' []  = []
mergeSrt' [x] = [x]
mergeSrt' x   = merge (mergeSrtA' a) (mergeSrtA' b)
                where (a,b) = split x

-- assertive version of divide and conquer merge sort
mergeSrtA' :: Ord a => [a] -> [a]
mergeSrtA' = post1  sorted
          $ assert1 intersectYieldsItself
          $ mergeSrt'

-- Exercise 3
-- The goal of this exercise is to extend the sudoku program from the course notes
-- with functions that can also handle sudokus of a special kind: the sudokus that
-- appear in NRC-Handelsblad each week (designed by Peter Ritmeester, from Oct
-- 8, 2005 onward). These NRC sudokus are special in that they have to satisfy a
-- few extra constraints: in addition to the usual sudoku constraints, each of the
-- 3x3 subgrids with left-top corner (2,2), (2,6), (6,2), and (6,6) should also yield a
-- injective function.

-- Formal statement of NRC constraint:
---- Pre and postcondition
---- Sudoku constraints +
---- For all 3x3 blocks with top-left at [(2,2), (2,6), (6,2), (6,6)];
----   For all non-empty values:
----     All values in block are different
----     All values in block are between 1 ... 9

-- To test the solver and difficulty rating use one of the following statements:
-- solveAndShowNormal example1
-- solveAndShowNormal onlineExampleEasy
-- solveAndShowNormal onlineExampleMedium
-- solveAndShowNormal onlineExampleExtreme
-- solveAndShowNrc nrcExampleEasy
-- solveAndShowNrc nrcExampleMedium
-- solveAndShowNrc nrcExampleHard

-- The difficulty rating is an estimation based on the number of unit constraints (cells that have 
-- only one option) versus cells that have two or more options. A sigmoid function is applied to 
-- show a nicer distribution.

-- The NRC sudoku problem given by exercise.
nrcexample :: Grid
nrcexample= [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

nrcsolution = solveAndShowNrc nrcexample

{-

Solution to NRC Sudoku given in exercise using solveAndShowNrc:
 +---------+---------+---------+
 | 4  7  8 | 3  9  2 | 6  1  5 |
 |   +-----|--+   +--|-----+   |
 | 6 |1  9 | 7| 5 |8 | 3  2| 4 |
 | 2 |3  5 | 4| 1 |6 | 9  7| 8 |
 +---------+---------+---------+
 | 7 |2  6 | 8| 3 |5 | 1  4| 9 |
 |   +-----|--+   +--|-----+   |
 | 8  9  1 | 6  2  4 | 7  5  3 |
 |   +-----|--+   +--|-----+   |
 | 3 |5  4 | 9| 7 |1 | 2  8| 6 |
 +---------+---------+---------+
 | 5 |6  7 | 2| 8 |9 | 4  3| 1 |
 | 9 |8  3 | 1| 4 |7 | 5  6| 2 |
 |   +-----|--+   +--|-----+   |
 | 1  4  2 | 5  6  3 | 8  9  7 |
 +---------+---------+---------+
-}

-- Exercise 4
-- To test, use randomNrc and randomNormal which generates a problem, solves it, and shows.
{-

Generated NRC solution:
+---------+---------+---------+
| 3  5  6 | 4  1  8 | 2  9  7 |
|   +-----|--+   +--|-----+   |
| 1 |2  7 | 5| 9 |3 | 8  6| 4 |
| 8 |4  9 | 6| 7 |2 | 5  1| 3 |
+---------+---------+---------+
| 5 |1  3 | 8| 2 |4 | 9  7| 6 |
|   +-----|--+   +--|-----+   |
| 4  9  2 | 7  6  5 | 1  3  8 |
|   +-----|--+   +--|-----+   |
| 7 |6  8 | 9| 3 |1 | 4  2| 5 |
+---------+---------+---------+
| 9 |7  5 | 1| 4 |6 | 3  8| 2 |
| 6 |3  4 | 2| 8 |9 | 7  5| 1 |
|   +-----|--+   +--|-----+   |
| 2  8  1 | 3  5  7 | 6  4  9 |
+---------+---------+---------+

Minimal NRC Sudoku problem for above solution:
+---------+---------+---------+
|       6 |         | 2  9    |
|   +-----|--+   +--|-----+   |
|   |   7 | 5|   |3 |     | 4 |
|   |     |  |   |  | 5   |   |
+---------+---------+---------+
|   |     |  |   |  |    7|   |
|   +-----|--+   +--|-----+   |
|         | 7  6    |         |
|   +-----|--+   +--|-----+   |
|   |     |  |   |  | 4   |   |
+---------+---------+---------+
|   |     |  |   |6 | 3   |   |
|   |     | 2| 8 |  |     |   |
|   +-----|--+   +--|-----+   |
|         |         |       9 |
+---------+---------+---------+
-}

-- Exercise 5
-- We can test whether a Sudoku is minimal by keep removing values until there
-- is more than one solution.
sudokuIsMinimal :: RuleSet -> Node -> Bool
sudokuIsMinimal rs n = and [not $ uniqueSol rs $ eraseN rs n p | p <- xs]
                        where xs = filledPositions (fst n)
