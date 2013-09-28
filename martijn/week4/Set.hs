module Set
where

import Data.List
import System.Random

import Week4
import SetOrd

listsize = 5
minrand  = 0
maxrand  = 10

-- generates a random Int between n and m (inclusive)
getRandomInt :: Int -> Int -> IO Int
getRandomInt n m = getStdRandom (randomR (n, m))

-- generates a list of random Ints
genIntList :: IO [Int]
genIntList = sequence $ (take listsize stream)
           where stream = (getRandomInt minrand maxrand) : stream

-- generates a random set
genRandomSet :: IO (Set Int)
genRandomSet = do
            m <- getRandomInt 0 3
            case m of
                0 -> do return emptySet
                _ -> do
                    ls <- genIntList
                    return (list2set ls)


-- unionSet already defined in SetOrd module

-- intersectSet gives the intersection of two Sets
intersectSet :: Set Int -> Set Int -> Set Int
intersectSet (Set xs) (Set ys) = list2set (intersect xs ys)

-- diffSet gives the non-associative difference between two sets
diffSet :: Set Int -> Set Int -> Set Int
diffSet (Set xs) (Set ys) = list2set (xs \\ ys)

-- the union set of two sets A and B contains all elements of A and all elements of B
unionProperty :: Set Int -> Set Int -> Set Int -> Bool
unionProperty (Set xs) (Set ys) setZ = (all (\ x -> (inSet x setZ)) xs) && (all (\ y -> (inSet y setZ)) ys)

-- the intersection of two sets A and B is the set that contains all elements that are elements of both A and B
intersectProperty :: Set Int -> Set Int -> Set Int -> Bool
intersectProperty setX setY (Set zs) = (all (\ z -> (inSet z setX) && (inSet z setY)) zs)

-- the difference of two sets A and B is the set of all elements of A that are not elements of B
diffProperty :: Set Int -> Set Int -> Set Int -> Bool
diffProperty (Set xs) setY (Set zs) = (filter (\ x -> (not (inSet x setY))) xs) == zs

-- tests a set property for random sets
-- iterations -> (operation) -> (testproperty) -> name -> round
testProperty :: Int -> (Set Int -> Set Int -> Set Int) -> (Set Int -> Set Int -> Set Int -> Bool) -> [Char] -> Int -> IO ()
testProperty n _ _ s 0 = do print (show n ++ " tests passed for " ++ show s)
testProperty n f p s r = do
                setX <- genRandomSet
                setY <- genRandomSet
                let setZ = (f setX setY)
                if (p setX setY setZ) then do
                    print ("OK: " ++ show setX ++ " and " ++ show setY ++ " " ++ show s ++ " = " ++ show setZ)
                    testProperty n f p s (r-1)
                else
                    error ("test failed on: " ++ show setX ++ " and " ++ show setY ++ " " ++ show s ++ " = " ++ show setZ)

-- test cases
testcases = [(unionSet, unionProperty, "UNION"),
             (intersectSet, intersectProperty, "INTERSECT"),
             (diffSet, diffProperty, "-")]

-- tests all cases n times
testSets :: Int -> IO ()
testSets n = mapM_ (\ (f, p, s) -> testProperty n f p s n ) testcases



-- Relations
-- binary relation type
type Rel a = [(a,a)]

-- 'on' operator for relations
infixr 5 @@

-- 'on' is defined for operands of type relation Rel and yields a new relation Rel
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

-- inserts an element in a list
insertList :: Ord a => a -> [a] -> [a]
insertList x []         = [x]
insertList x ys@(y:ys') = case compare x y of
                               GT -> y : insertList x ys'
                               EQ -> ys
                               _  -> x : ys

-- inserts an element in a relation
insertRel :: (Ord a) => a -> [a] -> [a]
insertRel x r = insertList x r

-- computes the union for relations
unionRel :: Ord a => Rel a -> Rel a -> Rel a
unionRel [] y     = y
unionRel (x:xs) y = insertRel x (unionRel xs y)

-- compute the transitive closure R+ of a relation R
trClos :: Ord a => Rel a -> Rel a
trClos r = unionRel r (r @@ r)


