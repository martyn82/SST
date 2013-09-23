module Set
where

import Data.List (intersect, (\\))
import System.Random

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
            m <- getRandomInt 0 1
            case m of
                0 -> do return emptySet
                1 -> do
                    ls <- genIntList
                    return (list2set ls)


-- unionSet already defined in SetOrd module

-- intersectSet gives the intersection of two Sets
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = list2set (intersect xs ys)

-- diffSet gives the non-associative difference between two sets
diffSet :: (Ord a) => Set a -> Set a -> Set a
diffSet (Set xs) (Set ys) = list2set (xs \\ ys)

-- the union set of two sets A and B contains all elements of A and all elements of B
unionProperty :: (Ord a) => Set a -> Set a -> Set a -> Bool
unionProperty (Set xs) (Set ys) setZ = (all (\ x -> (inSet x setZ)) xs) && (all (\ y -> (inSet y setZ)) ys)

-- tests the union function on sets
testUnion :: Int -> IO ()
testUnion 0 = do print ("all tests passed")
testUnion n = do
            s1 <- genRandomSet
            s2 <- genRandomSet
            let s = (unionSet s1 s2)
            if (unionProperty s1 s2 s) then do
                print ("test passed on: " ++ show s1 ++ " UNION " ++ show s2 ++ " = " ++ show s)
                testUnion (n-1)
            else
                error ("test failed on: " ++ show s1 ++ " UNION " ++ show s2 ++ " = " ++ show s)

-- the intersection of two sets A and B is the set that contains all elements that are elements of both A and B
intersectProperty :: (Ord a) => Set a -> Set a -> Set a -> Bool
intersectProperty setX setY (Set zs) = (all (\ z -> (inSet z setX) && (inSet z setY)) zs)

-- tests the intersection function on sets
testIntersect :: Int -> IO ()
testIntersect 0 = do print ("all tests passed")
testIntersect n = do
                s1 <- genRandomSet
                s2 <- genRandomSet
                let s = (intersectSet s1 s2)
                if (intersectProperty s1 s2 s) then do
                    print ("test passed on: " ++ show s1 ++ " INTERSECT " ++ show s2 ++ " = " ++ show s)
                    testIntersect (n-1)
                else
                    error ("test failed on: " ++ show s1 ++ " INTERSECT " ++ show s2 ++ " = " ++ show s)

-- the difference of two sets A and B is the set of all elements of A that are not elements of B
diffProperty :: (Ord a) => Set a -> Set a -> Set a -> Bool
diffProperty (Set xs) setY (Set zs) = (filter (\ x -> (not (inSet x setY))) xs) == zs

-- tests the difference function on sets
testDiff :: Int -> IO ()
testDiff 0 = do print ("all tests passed")
testDiff n = do
            s1 <- genRandomSet
            s2 <- genRandomSet
            let s = (diffSet s1 s2)
            if (diffProperty s1 s2 s) then do
                print ("test passed on: " ++ show s1 ++ " - " ++ show s2 ++ " = " ++ show s)
                testDiff (n-1)
            else
                error ("test failed on: " ++ show s1 ++ " - " ++ show s2 ++ " = " ++ show s)
