module Week4 
where

import SetOrd;
import System.Random;
import Data.List (nub, sort, delete);








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
                  
intersection, difference, union :: Ord a => (Set a) -> (Set a) -> (Set a)
intersection (Set x)  (Set y)  = list2set $ listIntersection x y
    where listIntersection (x:xs) (y:ys) | x < y     = listIntersection xs (y:ys)
                                         | x > y     = listIntersection (x:xs) ys
                                         | otherwise = x: listIntersection xs ys
          listIntersection _     _     = []

union (Set x) (Set y) = list2set $ x ++ y

difference (Set x)  (Set  y)  = list2set $ listDifference x y
    where listDifference  (x:xs) ys      | elem x ys =   listDifference xs ys
                                         | otherwise = x:listDifference xs ys
          listDifference   _      _      = []
          
          
          
unionContainsAll, unionContainsOnly :: Ord a => (Set a) -> (Set a) -> (Set a) -> Bool
unionContainsAll u (Set a) (Set b)  = (all (flip inSet $ u) a) 
                                    && (all (flip inSet $ u) b)
unionContainsOnly (Set u) a b       = all (\x -> inSet x a || inSet x b) u

differenceContainsNoRightItems, differenceContainsOnlyLeftItems :: Ord a => (Set a) -> (Set a) -> (Set a) -> Bool
differenceContainsNoRightItems  d _ (Set b)  = not $ any (flip inSet $ d) b
differenceContainsOnlyLeftItems (Set d) a _  = all (flip inSet $ a) d

intersectContainsOnlyItemsInBoth (Set i) a b = all (flip inSet $ a) i && all (flip inSet $ b) i

testPropertyOnRandomSet f p name = do   a <- getRandomSet
                                        b <- getRandomSet
                                        let c = (f a b)
                                        if (not $ p c a b) then do 
                                            error ("test failed for " ++ name ++ " on " ++ show a ++ " and " ++ show b)
                                        else return ()

testPropertyOnRandomSets _ _ name n 0 = do print (show n ++ " cases passed for '" ++ name ++ "'")
testPropertyOnRandomSets f p name n i = do testPropertyOnRandomSet  f p name
                                           testPropertyOnRandomSets f p name n (i - 1)
          

testSetCases = [(union, unionContainsOnly, "union contains only elements of a or b"),
             (union, unionContainsAll,  "union contains all elements of a and b"),
             (difference, differenceContainsNoRightItems, "difference contains no elements of b"),
             (difference, differenceContainsOnlyLeftItems, "difference contains only elements of a"),
             (intersection, intersectContainsOnlyItemsInBoth, "intersection contains only elements that are elements of both a and b")]
             
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



                                           