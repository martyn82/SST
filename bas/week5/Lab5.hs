module Lab5

where

import Week5;
import GenericSudoku;
import RandomSudoku;
import Data.List;


mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt [x] = [x]
mergeSrt (x:xs) = merge [x] (mergeSrt xs) 

-- Excercise 1
-- Find a suitable assertion, and write an assertive version of this.
-- Deliverables: Assertion, Haskell program that uses this assertion, indication of time
-- spent

intersectYieldsItself :: (Ord a) => [a] -> [a] -> Bool
intersectYieldsItself a b = intersect a b == a

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = post1   sorted "output should be sorted" 
          $ assert1 intersectYieldsItself "output should contain all elements from the input" 
          $ mergeSrt

          
-- Excercise 2
-- Another approach to merge sort is to start by splitting the list to be sorted in equal
-- parts, recursively sort the parts, next merge.
-- Implement this, using the following split function.

-- Next, find a suitable assertion, and write an assertive version.
-- Deliverables: Haskell program, assertion, assertive version of Haskell program that
-- uses this assertion, indication of time spent.

split :: [a] -> ([a],[a])
split xs = let
            n = (length xs) `div` 2
            in
            (take n xs, drop n xs)
            
            
            
mergeSrt' :: (Ord a) => [a] -> [a]
mergeSrt' []  = []
mergeSrt' [x] = [x]
mergeSrt' x   = merge (mergeSrtA' a) (mergeSrtA' b)
                where (a,b) = split x
                
                
mergeSrtA' :: Ord a => [a] -> [a]
mergeSrtA' = post1   sorted "output should be sorted" 
          $ assert1 intersectYieldsItself "output should contain all elements from the input" 
          $ mergeSrt'
 
-- Excercise 3
-- The goal of this exercise is to extend the sudoku program from the course notes
-- with functions that can also handle sudokus of a special kind: the sudokus that
-- appear in NRC-Handelsblad each week (designed by Peter Ritmeester, from Oct
-- 8, 2005 onward). These NRC sudokus are special in that they have to satisfy a
-- few extra constraints: in addition to the usual sudoku constraints, each of the
-- 3x3 subgrids with left-top corner (2,2), (2,6), (6,2), and (6,6) should also yield a
-- surjective function.

-- To test the solver and difficulty rating use one of the following statements:
-- solveAndShowNormal example1
-- solveAndShowNormal onlineExampleEasy
-- solveAndShowNormal onlineExampleMedium
-- solveAndShowNormal onlineExampleExtreme
-- solveAndShowNrc nrcExampleEasy
-- solveAndShowNrc nrcExampleMedium
-- solveAndShowNrc nrcExampleHard

-- the difficulty rating is an estimation based on the number of unit constraints (cells that have 
-- only one option) versus cells that have two or more options. A sigmoid function is applied to 
-- show a nicer distribution.

-- Excercise 4
-- To test, use randomNrc and randomNormal




          
