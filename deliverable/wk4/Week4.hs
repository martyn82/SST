module Week4 
where

import SetOrd;
import System.Random;
import Data.List (intersect, nub, sort, delete, (\\));

-- 2. Implement a random data generator for the datatype Set Int, where
-- Set is as defined in http://homepages.cwi.nl/~jve/rcrh/SetOrd.
-- hs.
-- (Deliverables: Haskell program, indication of time spent.)

--random stream for ease of use
randomSequence :: [IO Int]
randomSequence = repeat (randomRIO (1,10))

getRandomSet :: IO (Set Int)
getRandomSet = do ls <- sequence (take 10 randomSequence)
                  return (list2set ls)

                  
--set union already defined in SetOrd.hs

--intersection:
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) _     = Set []
intersectSet (Set (x:xs)) s | inSet x s  = insertSet x (intersectSet (Set xs) s)
                            | otherwise  = intersectSet (Set xs) s

--in a commercial environment we would do this:
intersectSet' :: (Ord a) => Set a -> Set a -> Set a
intersectSet' (Set xs) (Set ys) = list2set (intersect xs ys)

-- differenceSet gives the non-associative difference between two sets
differenceSet (Set x)  (Set  y)  = list2set $ listDifference x y
    where listDifference  (x:xs) ys      | elem x ys =   listDifference xs ys
                                         | otherwise = x:listDifference xs ys
          listDifference   _      _      = []

--in a commercial environment we would do this:
differenceSet' :: Set Int -> Set Int -> Set Int
differenceSet' (Set xs) (Set ys) = list2set (xs \\ ys)
          
          
          
shouldContainAll, shouldContainOnly, shouldBeCommutative, shouldNotBeCommutative, shouldContainSubsets, identityShouldYieldItself :: Ord a => (Set a -> Set a -> Set a) -> (Set a) -> (Set a) -> Bool
shouldContainAll u (Set a) (Set b)  = (all (flip inSet $ ab) a) 
                                    && (all (flip inSet $ ab) b)
                            where ab = u (Set a) (Set b)
                              
shouldContainOnly u a b  = all (\x -> inSet x a || inSet x b) ab
                            where ab = set2list $ u a b

set2list (Set a) = a                            
                            
shouldBeCommutative   u a b = ab == ba
                             where ab = u a b
                                   ba = u b a

shouldNotBeCommutative u a b = not $ shouldBeCommutative u a b
                                   
shouldContainSubsets u a b = (subSet a ab) && (subSet b ab)
                             where ab = u a b

identityShouldYieldItself u a b = aa == a && bb == b
                             where aa = u a a
                                   bb = u b b

shouldYieldItselfWhenCombinedWithEmptySet u a b = a == a'&& b == b'
                             where  a' = u a (Set [])
                                    b' = u b (Set [])
                                   
identityShouldYieldAnEmptySet d a b = a' == a && aa == (Set []) && b' == b && bb == (Set [])
                                  where a' = d a (Set [])
                                        aa = d a a
                                        b' = d b (Set [])
                                        bb = d b b
                                        
shouldContainNoRightItems  f a (Set b)  = not $ any (flip inSet $ d) b
                                        where d = f a (Set b)
shouldContainOnlyLeftItems f a b  = all (flip inSet $ a) d
                                        where d = set2list $ f a b

shouldContainOnlyItemsInBoth f a b = all (flip inSet $ a) i && all (flip inSet $ b) i
                                        where i = set2list $ f a b

testPropertyOnRandomSet f p name = do   a <- getRandomSet
                                        b <- getRandomSet
                                        if (not $ p f a b) then do 
                                            error ("test failed for " ++ name ++ " on " ++ show a ++ " and " ++ show b)
                                        else return ()

testPropertyOnRandomSets _ _ name n 0 = do print (show n ++ " cases passed for '" ++ name ++ "'")
testPropertyOnRandomSets f p name n i = do testPropertyOnRandomSet  f p name
                                           testPropertyOnRandomSets f p name n (i - 1)
          

testSetCases = [(unionSet, shouldContainOnly, "union contains only elements of a or b"),
                (unionSet, shouldContainAll,  "union contains all elements of a and b"),
                (unionSet, shouldBeCommutative,  "union should be commutative"),
                (unionSet, shouldContainSubsets,  "a should be a subset of ab, b should be a subset of ab"),
                (unionSet, shouldYieldItselfWhenCombinedWithEmptySet, "union with empty set should yield the original"),
                (unionSet, identityShouldYieldItself,  "union of a and a should be a, union of b and b should be b "),
                (differenceSet, shouldContainNoRightItems, "difference contains no elements of b"),
                (differenceSet, shouldContainOnlyLeftItems, "difference contains only elements of a"),
                (differenceSet, identityShouldYieldAnEmptySet, "a-a should yield an empty set"),
                (differenceSet', shouldContainNoRightItems, "difference contains no elements of b"),
                (differenceSet', shouldContainOnlyLeftItems, "difference contains only elements of a"),
                (differenceSet', identityShouldYieldAnEmptySet, "a-a should yield an empty set"),
                (intersectSet, shouldBeCommutative,  "intersection should be commutative"),
                (intersectSet, shouldContainOnlyItemsInBoth, "intersection contains only elements that are elements of both a and b"),
                (intersectSet, identityShouldYieldItself, "intersection identity should yield itself"),
                (intersectSet', shouldBeCommutative,  "intersection should be commutative"),
                (intersectSet', shouldContainOnlyItemsInBoth, "intersection contains only elements that are elements of both a and b"),
                (intersectSet', identityShouldYieldItself, "intersection identity should yield itself")]
             
runTestSetCase n (f,p,name) = testPropertyOnRandomSets f p name n n

runAllTestSetCases = mapM_ (runTestSetCase $ 1000) testSetCases


type Rel a = [(a,a)]
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

unionRelation :: (Ord a) => Rel a -> Rel a -> Rel a
unionRelation a  b = sort $ (nub $ a ++ b)

--minimal transitive closure
transitiveClosure :: (Ord a) => (Rel a) -> (Rel a)
transitiveClosure a = unionRelation a (a @@ a)

--everything that can be done in two steps can be done in one step
isTransitive :: (Ord a) => Rel a -> Bool
isTransitive a = (sort $ nub [(x,z) | (x,y) <- a, (x', z) <- (a @@ a), x == x']) == (sort $ nub $ a)

isMinimal a    = all (not.isTransitive) [delete x a | x <- a, (delete x a) /= []]

randomRelation = do a <- sequence (take 3 randomSequence)
                    b <- sequence (take 3 randomSequence)
                    return $ nub $ zip a b

testRelationCases = [(transitiveClosure, isMinimal,    "transitive closure should be minimal"),
                     (transitiveClosure, isTransitive, "transitive closure should yield a transitive relation")]

testPropertyOnRandomRelation  f p name = do a <- randomRelation
                                            let b = f a
                                            if (not $ p b) then do 
                                                error ("test failed for " ++ name ++ " on " ++ show a ++ " -> " ++ show b)                          
                                            else return ()
                                  
testPropertyOnRandomRelations _ _ name n 0 = do print (show n ++ " cases passed for '" ++ name ++ "'")
testPropertyOnRandomRelations f p name n i = do testPropertyOnRandomRelation  f p name
                                                testPropertyOnRandomRelations f p name n (i - 1)
                                                
runTestRelationCase n (f, p, name) = testPropertyOnRandomRelations f p name n n  
                          
runAllTestRelationCases = mapM_ (runTestRelationCase $ 10) testRelationCases                 

--test results: minimum property fails on (2,2),(8,8)
--test failed for transitive closure should yield a
--transitive relation on [(6,1),(1,7),(10,7)] -> [(1,7),(6,1),(6,7),(10,7)]



                                           