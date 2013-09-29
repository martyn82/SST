module Week4 
where

import SetOrd;
import System.Random;
import Data.List (union, intersect, nub, sort, delete, (\\), subsequences);

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

-- TEST RESULTS:
--"1000 cases passed for 'union contains only elements of a or b'"
--"1000 cases passed for 'union contains all elements of a and b'"
--"1000 cases passed for 'union should be commutative'"
--"1000 cases passed for 'a should be a subset of ab, b should be a subset of ab'"
--"1000 cases passed for 'union with empty set should yield the original'"
--"1000 cases passed for 'union of a and a should be a, union of b and b should be b '"
--"1000 cases passed for 'difference contains no elements of b'"
--"1000 cases passed for 'difference contains only elements of a'"
--"1000 cases passed for 'a-a should yield an empty set'"
--"1000 cases passed for 'difference contains no elements of b'"
--"1000 cases passed for 'difference contains only elements of a'"
--"1000 cases passed for 'a-a should yield an empty set'"
--"1000 cases passed for 'intersection should be commutative'"
--"1000 cases passed for 'intersection contains only elements that are elements of both a and b'"
--"1000 cases passed for 'intersection identity should yield itself'"
--"1000 cases passed for 'intersection should be commutative'"
--"1000 cases passed for 'intersection contains only elements that are elements of both a and b'"
--"1000 cases passed for 'intersection identity should yield itself'"

-- Relations
-- binary relation type
type Rel a = [(a,a)]

-- 'on' operator for relations
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- compute the transitive closure R+ of a relation R
trClos :: Ord a => Rel a -> Rel a
trClos r | isTransitive [] r = sort r
         | otherwise         = trClos (union r (r @@ r))

-- property test functions
isTransitive, isSubset, isMinimal, hasSameDomain, hasSameRange :: Ord a => Rel a -> Rel a -> Bool

-- checks if transitivity holds for the given relation
isTransitive _ r = and [elem (x,z) r | (x,y) <- r, (y',z) <- r, y == y']

-- trClos must not remove pairs from the original relation
isSubset r1 r2 = subSet (list2set r1) (list2set r2)

-- trClos must create a minimal transitive relation
-- if one of the added pairs is removed, transitivity must not hold anymore.
isMinimal r1 r2 = and [not (isTransitive [] (r1 ++ r1')) | r1' <- (init . subsequences) (r2 \\ r1)]

-- transitive closure must have the same set of first elements as its original
hasSameDomain r1 r2 = dr == dfr
                       where dr = sort $ nub [x | (x, _) <- r1]
                             dfr = sort $ nub [x' | (x', _) <- r2]

-- transitive closure must have the same set of second elements as its original
hasSameRange r1 r2 = rr == rfr
                      where rr = sort $ nub [y | (_, y) <- r1]
                            rfr = sort $ nub [y' | (_, y') <- r2]

-- generates a random relation
randomRelation :: IO [(Int, Int)]
randomRelation = do
                    a <- sequence (take 3 randomSequence)
                    b <- sequence (take 3 randomSequence)
                    return $ nub $ zip a b

-- testcases
testRelationCases = [(trClos, isTransitive, "transitive closure should yield a transitive relation"),
                     (trClos, isMinimal, "transitive closure should be the minimum transitive relation"),
                     (trClos, isSubset, "the original relation R should be a subset of the transitive closure R+"),
                     (trClos, hasSameDomain, "transitive closure R+ should have the same set of first elements of each pair as R"),
                     (trClos, hasSameRange, "transitive closure R+ should have the same set of second elements of each pair as R")]

-- test relation property
testPropertyOnRandomRelation f p name = do
                                           a <- randomRelation
                                           let b = f a
                                           if (not $ p a b) then do 
                                               error ("test failed for '" ++ name ++ "' on " ++ show a ++ " -> " ++ show b)                          
                                           else return ()

-- test property for random relations
testPropertyOnRandomRelations _ _ name n 0 = do print (show n ++ " cases passed for '" ++ name ++ "'")
testPropertyOnRandomRelations f p name n i = do testPropertyOnRandomRelation  f p name
                                                testPropertyOnRandomRelations f p name n (i - 1)

-- run test case for relations
runTestRelationCase n (f, p, name) = testPropertyOnRandomRelations f p name n n

-- run all relations test cases
runAllTestRelationCases :: IO ()
runAllTestRelationCases = mapM_ (runTestRelationCase $ 1000) testRelationCases                 

-- TEST RESULTS:
--"1000 cases passed for 'transitive closure should yield a transitive relation'"
--"1000 cases passed for 'transitive closure should be the minimum transitive relation'"
--"1000 cases passed for 'the original relation R should be a subset of the transitive closure R+'"
--"1000 cases passed for 'transitive closure R+ should have the same set of first elements of each pair as R'"
--"1000 cases passed for 'transitive closure R+ should have the same set of second elements of each pair as R'"

