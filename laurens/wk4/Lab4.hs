module Lab4 where

import Week4
import SetOrd

import System.Random

-- Exercise 2: Time spent 1 hour
-- genIntList from wk 3
getRandomInt :: IO Int
getRandomInt = getStdRandom random

genIntList :: Int -> IO [Int]
genIntList n = sequence $ (take n stream)
                   where stream = getRandomInt : stream

-- getRandomInt' ((#minValue, #maxValue))
getRandomInt' :: (Int, Int) -> IO Int
getRandomInt' n = getStdRandom (randomR n)

-- genIntList' (#lists) ((#minValue, #maxValue))
genIntList' :: Int -> (Int, Int) -> IO [Int]
genIntList' n m = sequence $ (take n stream)
                      where stream = (getRandomInt' m) : stream

-- gets a random set with max 10 elements (depends on the randomness of genIntList)
getRandomSet :: IO (Set Int)
getRandomSet = do xs <- genIntList 10
                  return (list2set xs)

-- getRandomSet' (#max length of set) ((#minValue, #maxValue))
getRandomSet' :: Int -> (Int, Int) -> IO (Set Int)
getRandomSet' l r = do xs <- genIntList' l r
                       return (list2set xs)

-- getRandomSets (#sets) (#max length of set) (#minValue, #maxValue)
getRandomSets :: Int -> Int -> (Int, Int) -> IO [(Set Int)]
getRandomSets 0 _ _ = return []
getRandomSets n l r = do x <- getRandomSet' l r
                         xs <- getRandomSets (n-1) l r
                         return (x:xs)

-- Exercise 3:
-- Set union: A union B = x in A or x in B
-- unionSet :: (Ord a) => Set a -> Set a -> Set a
-- unionSet (Set [])     set2  = set2
-- unionSet (Set (x:xs)) set2  = insertSet x (unionSet (Set xs) set2)

-- Set intersection: A intersect B = x in A and x in B
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) _     = Set []
intersectSet (Set (x:xs)) s | inSet x s  = insertSet x (intersectSet (Set xs) s)
                            | otherwise  = intersectSet (Set xs) s

-- Set difference: A - B = x in A and x not in B
differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet s (Set [])     = s
differenceSet s (Set (y:ys)) | inSet y s = differenceSet (deleteSet y s) (Set ys)
                             | otherwise = differenceSet s (Set ys)

-- union properties:
-- all in A are in union A B and all in B are in union A B
-- testSetUnionElements (Union method) (Set A) (Set B)
testSetUnionElements :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetUnionElements u (Set a) (Set b) = all (`inSet` ab) [a' | a' <- a] && all (`inSet` ab) [b' | b' <- b]
                                         where ab = u (Set a) (Set b)

-- length of union A B >= length of A and length of union A B >= length of B
-- testSetUnionLength (Union method) (Set A) (Set B)
testSetUnionLength :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetUnionLength u (Set a) (Set b) = length a <= length ab && length b <= length ab
                                       where (Set ab) = u (Set a) (Set b)

-- union A B = union B A
-- testSetUnionEquality (Union method) (Set A) (Set B)
testSetUnionEquality :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetUnionEquality u a b = ab == ba
                             where ab = u a b
                                   ba = u b a

-- union A Set [] = A
-- union A A = A
-- testSetUnionIdentity (Union method) (Set A) (Set B)
testSetUnionIdentity :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetUnionIdentity u a b = a' == a && aa == a && b' == b && bb == b
                             where a' = u a (Set [])
                                   aa = u a a
                                   b' = u b (Set [])
                                   bb = u b b

-- testSetUnion (# tests) (# max set length) (#minValue,#maxValue)
testSetUnion :: Int -> Int -> (Int, Int) -> IO ()
testSetUnion n l r = do xs <- getRandomSets n l r
                        ys <- getRandomSets n l r
                        testSet n unionSet [testSetUnionElements, testSetUnionLength, testSetUnionEquality, testSetUnionIdentity] xs ys

-- testSet (#tests) (set method) [method properties] [set method input A] [set method input B]
testSet :: Int -> (Set Int -> Set Int -> Set Int) -> [((Set Int -> Set Int -> Set Int) -> Set Int -> Set Int -> Bool)] -> [Set Int] -> [Set Int] -> IO ()
testSet n _ _ _ []           = print (show n ++ " tests passed")
testSet n u ps (x:xs) (y:ys) = if all (&& True) [p u x y | p <- ps]
                               then do print ("pass on: " ++ show x ++ ", " ++ show y)
                                       testSet n u ps xs ys
                               else error ("failed test on: " ++ show x ++ ", " ++ show y)
