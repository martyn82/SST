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
-- the test

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

-- Retrieves the smallest composite number that has passed the Fermat's primality check after testing n times.
-- It turns out, if n is large enough (± 10,000), the smallest composite number that passes the test is 4.
-- The larger k, the larger the smallest number will be, but it also takes a lot more time to complete the test.
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

-- Computes the carmichael numbers
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]


