module Lab4 where

import Week4
import SetOrd

import System.Random
import Data.List

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

-- Exercise 3 : Time spent 5 hours
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

-- intersect properties
-- all a in B and b in A are in intersection A B
-- testSetIntersectElements (Intersect method) (Set A) (Set B)
testSetIntersectElements :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetIntersectElements i (Set a) (Set b) = all (`inSet` ab) [a' | a' <- a, inSet a' (Set b)]
                                          && all (`inSet` ab) [b' | b' <- b, inSet b' (Set a)]
                                             where ab = i (Set a) (Set b)

-- length of intersection A B <= length of A and length of intersection A B <= length of B
-- testSetIntersectLength (Intersect method) (Set A) (Set B)
testSetIntersectLength :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetIntersectLength i (Set a) (Set b) = length a >= length ab && length b >= length ba
                                           where (Set ab) = i (Set a) (Set b)
                                                 (Set ba) = i (Set b) (Set a)

-- intersection A B = intersection B A
-- testSetIntersectEquality (Intersect method) (Set A) (Set B)
testSetIntersectEquality :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetIntersectEquality i a b = ab == ba
                                 where ab = i a b
                                       ba = i b a

-- intersection A Set [] = Set []
-- intersection A A = A
-- testSetIntersectIdentity (Intersect method) (Set A) (Set B)
testSetIntersectIdentity :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetIntersectIdentity i a b = a' == (Set []) && aa == a && b' == (Set []) && bb == b
                                 where a' = i a (Set [])
                                       aa = i a a
                                       b' = i b (Set [])
                                       bb = i b b

-- difference properties
-- all a not in B are in A - B
-- testSetDifferenceElements (Difference method) (Set A) (Set B)
testSetDifferenceElements :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetDifferenceElements d (Set a) (Set b) = all (`inSet` ab) [a' | a' <- a, not (inSet a' (Set b))]
                                           && all (`inSet` ba) [b' | b' <- b, not (inSet b' (Set a))]
                                              where ab = d (Set a) (Set b)
                                                    ba = d (Set b) (Set a)

-- length of difference A B <= length of A and length of difference A B <= length of B
-- testSetDifferenceLength (Difference method) (Set A) (Set B)
testSetDifferenceLength :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetDifferenceLength d (Set a) (Set b) = length a >= length ab && length b >= length ba
                                            where (Set ab) = d (Set a) (Set b)
                                                  (Set ba) = d (Set b) (Set a)

-- difference A Set [] = A
-- difference A A = Set []
-- testSetDifferenceIdentity (Difference method) (Set A) (Set B)
testSetDifferenceIdentity :: (Ord a) => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testSetDifferenceIdentity d a b = a' == a && aa == (Set []) && b' == b && bb == (Set [])
                                  where a' = d a (Set [])
                                        aa = d a a
                                        b' = d b (Set [])
                                        bb = d b b

-- testSetUnion (# tests) (# max set length) (#minValue,#maxValue)
testSetUnion :: Int -> Int -> (Int, Int) -> IO ()
testSetUnion n l r = do xs <- getRandomSets n l r
                        ys <- getRandomSets n l r
                        testSet n unionSet [testSetUnionElements, testSetUnionLength, testSetUnionEquality, testSetUnionIdentity] xs ys

-- testSetIntersect (# tests) (# max set length) (#minValue,#maxValue)
testSetIntersect :: Int -> Int -> (Int, Int) -> IO ()
testSetIntersect n l r = do xs <- getRandomSets n l r
                            ys <- getRandomSets n l r
                            testSet n intersectSet [testSetIntersectElements, testSetIntersectLength, testSetIntersectEquality, testSetIntersectIdentity] xs ys

-- testSetDifference (# tests) (# max set length) (#minValue,#maxValue)
testSetDifference :: Int -> Int -> (Int, Int) -> IO ()
testSetDifference n l r = do xs <- getRandomSets n l r
                             ys <- getRandomSets n l r
                             testSet n differenceSet [testSetDifferenceElements, testSetDifferenceLength, testSetDifferenceIdentity] xs ys

-- testSet (#tests) (set method) [method properties] [set method input A] [set method input B]
testSet :: Int -> (Set Int -> Set Int -> Set Int) -> [((Set Int -> Set Int -> Set Int) -> Set Int -> Set Int -> Bool)] -> [Set Int] -> [Set Int] -> IO ()
testSet n _ _ _ []           = print (show n ++ " tests passed")
testSet n u ps (x:xs) (y:ys) = if and [p u x y | p <- ps]
                               then do print ("pass on: " ++ show x ++ ", " ++ show y)
                                       testSet n u ps xs ys
                               else error ("failed test on: " ++ show x ++ ", " ++ show y)

-- Exercise 4 : Time spent 2 hour
type Rel a = [(a, a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

-- First attempt using list comprehensions.
trClos :: Ord a => Rel a -> Rel a
trClos r | null x    = sort r
         | otherwise = trClos (r ++ x)
         where x = [x | x <- r @@ r, not (elem x r)]

-- Method to create the smallest transitive closure (R+)
trClos' :: Ord a => Rel a -> Rel a
trClos' r | transR r  = r
          | otherwise = trClos' (union r (r @@ r))

transR :: Ord a => Rel a -> Bool
transR r = and [elem (x,z) r | (x,y) <- r, (y',z) <- r, y == y']

-- Exercise 5 :
-- trClos result should have transitive closure. This means that: "xRy and yRz -> xRz" must hold
-- given the applied list comprehension in transR, we can see that this holds.
--
-- I could retest it here, but this would mean writing the same test and effectively
-- testing the programming language. That is not to be tested.

-- trClos must not remove pairs from the original relation
testTrClosSubset :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
testTrClosSubset f r = subSet (list2set r) (list2set (f r))

-- trClos must create a minimal transitive relation
-- if one of the added pairs is removed, transitivity must not hold anymore.
testTrClosMinimal :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
testTrClosMinimal f r = and [not (transR (r ++ r')) | r' <- (init . subsequences) (tc \\ r)]
                        where tc = f r

-- Generate random test data
-- getRandomPair' ((#minValue, #maxValue))
getRandomPair' :: (Int, Int) -> IO (Int,Int)
getRandomPair' n = do x <- getRandomInt' n
                      y <- getRandomInt' n
                      return (x,y)

-- genPairList' (#list length) ((#minValue, #maxValue))
genPairList' :: Int -> (Int, Int) -> IO [(Int,Int)]
genPairList' n m = sequence $ (take n stream)
                      where stream = (getRandomPair' m) : stream

-- getRandomRelation' (#max relation list length) ((#minValue, #maxValue))
getRandomRelation' :: Int -> (Int, Int) -> IO (Rel Int)
getRandomRelation' l r = do l' <- getRandomInt' (0,l)
                            xs <- genPairList' l' r
                            return xs

-- getRandomRelations (#relations) (#max relation list length) (#minValue,#maxValue)
getRandomRelations :: Int -> Int -> (Int, Int) -> IO [(Rel Int)]
getRandomRelations 0 _ _ = return []
getRandomRelations n l r = do x <- getRandomRelation' l r
                              xs <- getRandomRelations (n-1) l r
                              return (x:xs)

-- testTrClos (# tests) (# max relation list length) (#minValue,#maxValue)
testTrClos :: Int -> Int -> (Int, Int) -> IO ()
testTrClos n l r = do xs <- getRandomRelations n l r
                      testTrClos' n trClos' [testTrClosSubset, testTrClosMinimal] xs

-- testTrClos' (#tests) (trClos method) [method properties] [trClos method input A]
testTrClos' :: Int -> (Rel Int -> Rel Int) -> [((Rel Int -> Rel Int) -> Rel Int -> Bool)] -> [Rel Int] -> IO ()
testTrClos' n _ _ []      = print (show n ++ " tests passed")
testTrClos' n f ps (r:rs) = if and [p f r | p <- ps]
                            then do print ("pass on: " ++ show r)
                                    testTrClos' n f ps rs
                            else error ("failed test on: " ++ show r)

-- Test report:
-- I did not find any unexpected behavior.
-- 
-- "pass on: [(3,3)]"
-- "pass on: []"
-- "pass on: [(1,1),(1,2),(2,3),(1,2),(3,0),(2,1),(2,1),(1,0)]"
-- "pass on: [(0,3),(1,3),(2,0)]"
-- "10000 tests passed"
