module Lab6
where

import Control.Monad
import Data.List
import System.Random

import Week6

-- Exercise 1.
-- Algorithm for modular exponentiation of x^y mod n in polynomial time (repeatedly squaring modulo n)
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1                          -- exponentiation by 0 always gives 1, by definition
exM' x y n = let
                z = exM' x (div y 2) n
                w = rem (z*z) n
             in
                if even y then w
                else rem (x*w) n

-- Exercise 2.
-- Found it hard to really prove automatically.
-- When doing some manual invocations and observe the response time, it is obvious that exM' is faster than expM, however.

-- Exercise 3.
-- Generates the list of composite numbers.
-- It takes the number part of all (integer,bool) pairs where the prime property is false
composites :: [Integer]
composites = map (\ (n,p) -> n) (filter (\ (n,p) -> (not p)) (sieve' [2..]))

-- characterize numbers by prime property
sieve' :: [Integer] -> [(Integer,Bool)]
sieve' ns =
        let
            ps = (sieve [2..])
        in
            map (\ n -> (n, (inlist n ps))) ns
        where
            inlist n (p:ps) | p < n     = inlist n ps
                            | p == n    = True
                            | otherwise = False

-- Exercise 4.
-- Tests the primeF function with composites and returns the first composite number that passes the Fermat's primality test
testF :: Int -> IO Integer
testF k = (testF' k composites)
    where testF' k (n:ns) = do
               s <- primeF k n
               if s then return n
               else (testF' k ns)

-- Retrieves the smallest composite number that passed the Fermat's primality check after testing n times.
-- It turns out, if n is large enough (± 10,000), the smallest composite number that passes the test is 4.
-- The larger k, the larger the smallest number will be, therefore the larger k, the more accurate the check.
-- But it also takes a lot more time to complete.
testFSmallest :: Integer -> Int -> IO Integer
testFSmallest n k = minM (testFSmallest' n k)
            where testFSmallest' 0 _ = []
                  testFSmallest' n k = ((testF k) : (testFSmallest' (n-1) k))

-- Retrieves the smallest number in a monadic list of integers.
minM :: [IO Integer] -> IO Integer
minM []          = error "empty list"
minM [x]         = x
minM (x:y:ys)    = do
                    a <- x
                    b <- y
                    if (a >= b) then minM (y:ys)
                    else minM (x:ys)

-- Exercise 5.
-- Computes the carmichael numbers.
-- Carmichael numbers have the same property as what Fermat first defined as a prime property, but they are not prime.
-- The numbers generated here should pass the Fermat's primality check.
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Tests the primeF Fermat's primality check against the carmichael (CM) numbers and returns the first number that passes the check.
-- For k={1,2,3}: mostly 294409 (first CM number), sometimes 56052361 (second CM number)
-- This could be due to the fact that the higher k the less likely it is that a carmichael number occurs.
testFCM :: Int -> IO Integer
testFCM k = (testF' k carmichael)
    where testF' k (n:ns) = do
                s <- primeF k n
                if s then return n
                else (testF' k ns)

-- Produces a list of carmichael numbers that passed the testFCM test
testFCMs :: Integer -> Int -> IO [Integer]
testFCMs n k = sequence $ testFCMs' n k
    where testFCMs' 0 _ = []
          testFCMs' n k = (testFCM k) : (testFCMs' (n-1) k)

-- Exercise 6.
-- Test the Miller-Rabin primality check with carmichael numbers.
-- There are still carmichael numbers that pass the Miller-Rabin primality check. It is very slow on larger k (even on k=3).
testMR :: Int -> IO Integer
testMR k = (testMR' k carmichael)
    where testMR' k (n:ns) = do
                        s <- primeMR k n
                        if s then return n
                        else (testMR' k ns)

-- Runs n tests against testMR
testMRs :: Integer -> Int -> IO [Integer]
testMRs n k = sequence $ testMRs' n k
        where testMRs' 0 _ = []
              testMRs' n k = (testMR k) : (testMRs' (n-1) k)

-- Exercise 7.
-- Tests whether 2^p-1 of prime p is also prime.
mersenne :: Integer -> IO Bool
mersenne 0 = error "p is not prime"
mersenne 1 = return True
mersenne p = do
           isp <- primeMR 1 p
           if isp then primeMR 1 (2^p-1)
           else error "p is not prime"

-- I have been naively pushing my machine to the max to find m26, however, my hardware wasn't sufficient.
-- After several reboots, I managed to confirm the following.
mersennes :: IO ()
mersennes = mersennes' [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25] 1
    where mersennes' [] _     = print "Done"
          mersennes' (p:ps) i = do
                            isprime <- primeMR 1 p
                            if isprime then do print ("OK: m" ++ show i)
                            else do print ("FAIL: m" ++ show i ++ " is NOT a prime!")
                            mersennes' ps (i+1)

-- Will list all mersenne primes starting with number p.
-- As long as memory is sufficient!!
findmersenne :: Integer -> IO ()
findmersenne p = findmersenne' (filter (> p) primes)
    where findmersenne' []     = print "Done" -- stop condition, even though we iterate over an infinite list
          findmersenne' (p:ps) = do
                            ismersenne <- mersenne p
                            when ismersenne (print ("2^" ++ show p ++ "-1 is Mersenne prime!"))
                            findmersenne' ps

