module Lab3 where

import Data.List
import Techniques
import Week3

-- Exercise 3 : Time spent 2 hours (hard to understand how to work with IO monad)
-- generates a list of 10 elements with random values in the range of 0-10.
genIntList :: IO [Int]
genIntList = getRandomInts 10 10

-- getRandomInts maxValue, amount, list of random ints.
getRandomInts :: Int -> Int -> IO [Int]
getRandomInts _ 0 = return []
getRandomInts m n = do x <- getRandomInt m
                       xs <- getRandomInts m (n-1)
                       return (x:xs)

-- Exercise 4 : Time spent 1 hour (had to find a somewhat efficient way to check this, because this method blows up because of the amount of permutations (n!))
-- precondition: list is finite.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | length xs /= length ys = False
                    | otherwise = isPermutation' xs ys

-- precondition: lists are of equal length.
isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation' [] [] = True
isPermutation' [] _ = False
isPermutation' (x:xs) ys | x `elem` ys = isPermutation' xs (delete x ys)
                         | otherwise = False

-- Exercise 5 : Time spent 4 hours (spent some time to identify some properties, spent a lot time on implementing the test methods)
-- Some list is a permutation of another list if the lists are of equal length
testPermutationLengthEquality :: Eq a => [a] -> [a] -> Bool
testPermutationLengthEquality xs ys = length xs == length ys

-- A list is a permutation of another list, if there is no difference between the two sets
testPermutationListDifference :: Eq a => [a] -> [a] -> Bool
testPermutationListDifference xs ys = (xs \\ ys) == [] && (ys \\ xs) == []

-- Previous also implies that the intersection of both lists, always yield the entire list.
testPermutationListIntersection :: Eq a => [a] -> [a] -> Bool
testPermutationListIntersection xs ys = (xs `intersect` ys) == xs && (ys `intersect` xs) == ys

-- If it is a permutation on some list, it also is a permutation on the sets of these lists.
testPermutationOnUniqueList :: Eq a => [a] -> [a] -> Bool
testPermutationOnUniqueList xs ys = isPermutation (nub xs) (nub ys)

-- test our permutation properties on n random lists.
-- testPermutations (# lists) (# maxValue) (property methods) -> ([Result of IsPermuation, Result of Properties], [Inputs])
testPermutations :: Int -> Int -> [([Int] -> [Int] -> Bool)] -> IO [([Bool], [[Int]])]
testPermutations 0 _ _ = return []
testPermutations n m props = do x <- testPermutations' m props
                                xs <- testPermutations (n-1) m props
                                return (x:xs)

-- testPermutations' (# maxValue) (property methods) -> ([Result of IsPermuation, Result of Properties], [Inputs])
testPermutations' :: Int -> [([Int] -> [Int] -> Bool)] -> IO ([Bool], [[Int]])
testPermutations' m props = do l1 <- getRandomInt 10
                               l2 <- getRandomInt 10
                               xs <- getRandomInts m l1
                               ys <- getRandomInts m l2
                               let b1 = isPermutation xs ys
                               let b2 = and [p xs ys | p <- props]
                               return ([b1, b2], [xs, ys])

-- testAllPermuationProperties (# tests) (# maxvalue)
testAllPermutationProperties n m = do r <- testPermutations n m [testPermutationLengthEquality, testPermutationListDifference, testPermutationListIntersection, testPermutationOnUniqueList]
                                      let r' = filter (\x -> foldr (/=) False (fst x)) r -- Select those result that said that isPermutation was true and properties was false or vice versa.
                                      return r'

-- Exercise 6 -> View CNFTest.hs

-- Exercise 7 : Time spent 2 hours
getRandomTerm :: IO Term
getRandomTerm = do n <- getRandomInt 1
                   case n of
                     0 -> do m <- getRandomInt 10
                             return (V (show (m+1)))
                     1 -> do m <- getRandomInt 10
                             o <- getRandomInt 3
                             ts <- getRandomTerms o
                             return (F (show (m+1)) ts)

getRandomTerms :: Int -> IO [Term]
getRandomTerms 0 = return []
getRandomTerms n = do t <- getRandomTerm
                      ts <- getRandomTerms (n-1)
                      return (t:ts)

getRandomFOLF :: IO Formula
getRandomFOLF = do d <- getRandomInt 4
                   getRandomFOLForm d
            
getRandomFOLForm :: Int -> IO Formula 
getRandomFOLForm 0 = do m <- getRandomInt 20
                        return (Atom (show (m+1)) [x, y, z])
getRandomFOLForm d = do n <- getRandomInt 8
                        case n of
                          0 -> do m <- getRandomInt 20
                                  t <- getRandomTerm
                                  return (Atom (show (m+1)) [t])
                          1 -> do t1 <- getRandomTerm
                                  t2 <- getRandomTerm
                                  return (Eq t1 t2)
                          2 -> do f <- getRandomFOLForm (d-1)
                                  return (Neg f)
                          3 -> do f <- getRandomFOLForm (d-1)
                                  g <- getRandomFOLForm (d-1)
                                  return (Impl f g)
                          4 -> do f <- getRandomFOLForm (d-1)
                                  g <- getRandomFOLForm (d-1)
                                  return (Equi f g)
                          5 -> do m  <- getRandomInt 5
                                  fs <- getRandomFOLForms (d-1) m
                                  return (Conj fs)
                          6 -> do m  <- getRandomInt 5
                                  fs <- getRandomFOLForms (d-1) m
                                  return (Disj fs)
                          7 -> do m  <- getRandomInt 20
                                  f <- getRandomFOLForm (d-1)
                                  return (Forall (show (m+1)) f)
                          8 -> do m  <- getRandomInt 20
                                  f <- getRandomFOLForm (d-1)
                                  return (Exists (show (m+1)) f)

getRandomFOLFs :: Int -> IO [Formula]
getRandomFOLFs n = do d <- getRandomInt 3
                      getRandomFOLForms d n     

getRandomFOLForms :: Int -> Int -> IO [Formula]
getRandomFOLForms _ 0 = return []
getRandomFOLForms d n = do 
                        f <- getRandomFOLForm d
                        fs <- getRandomFOLForms d (n-1) 
                        return (f:fs)
