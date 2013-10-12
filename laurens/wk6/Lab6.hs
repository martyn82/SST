module Lab6

where
import Data.List
import System.Random
import Week6

import Control.Monad

-- Exercise 1: Implement a fast modular exponentation. Time spent 5 hours in understanding modular arithmetic and implementing these algorithms
-- Idea 1, use bitwise representation of integer, and raise to each 2^p and mod that.
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ _ 1 = 0
exM' _ 0 _ = 1
exM' x y n = product [x^p `mod` n | p <- (allPowersToRaiseTo . decomp') y] `mod` n

-- Get all powers of 2 to raise to
allPowersToRaiseTo :: [Integer] -> [Integer]
allPowersToRaiseTo xs = [2^ (fst p) | p <- zip [0..] xs, snd p == 1]

-- Get all powers of 2 from integer (I'm missing bitwise operators here..)
decomp' :: Integer -> [Integer]
decomp' 0 = []
decomp' n = let (q,r) = n `divMod` 2
            in r : decomp' q

-- Idea 2, from wikipedia right to left binary method (was already implemented, and proves to be fastest)
--exM :: Integer -> Integer -> Integer -> Integer
--exM _ _ 1 = 0
--exM _ 0 _ = 1
--exM x y n = let 
--              z = exM x (y `div` 2) n
--              w = multM z z n
--            in 
--              if even y then w
--              else multM x w n

-- Exercise 2: Check that the implementation of exM is more efficient than that of expM. Time spent 1 hour.
-- The trick in the faster algorithm lays in doing smaller calculations which are performed faster
-- than large calculation. So the check efficiency. Just try some large numbers and view that expM is struggling while exM is instant.
modExpTest1a = expM m3 m4 m5
modExpTest1b = exM m3 m4 m5

modExpTest2a = expM m13 m14 m15
modExpTest2b = exM m13 m14 m15

-- Exercise 3: Change sieve to not throw away composites. Time spent: 3 hours.
composites :: [Integer]
composites = [c | (c,_) <- sieveC' (zip [2..] trues)]

trues = True : trues

-- Sieve with primes and composites.
sieve' ((n,False):ns) = (n,False) : sieve' ns
sieve' ((n,True):ns)  = (n,True)  : sieve' (map (mark n) ns)
                        where mark i (m,p) | p         = (m, rem m i /= 0)
                                           | otherwise = (m,p)

-- Sieve with composites only
sieveC' ((n,False):ns) = (n,False) : sieveC' ns
sieveC' ((n,True):ns)  = sieveC' (map (mark n) ns)
                         where mark i (m,p) | p         = (m, rem m i /= 0)
                                            | otherwise = (m,p)

-- Exercise 4: Try to fool Fermat's primality check. Time spent 1 hour.
testFs :: Int -> Int -> IO [Integer]
testFs n k = sequence $ take n stream
             where stream = (testF k composites) : stream

testF :: Int -> [Integer] -> IO Integer
testF k ns = testF' k ns

testF' :: Int -> [Integer] -> IO Integer
testF' k (n:ns) = do b <- primeF k n
                     if b
                     then return n
                     else testF' k ns

-- Test results:
-- As expected we got up to a higher composite number after doing more checks.
-- The check of Fermat's is quite lame however. If he would neglect the 1, he
-- would have stumbled upon 4 every time.
--
-- *Lab6> testFs 10 1
-- [4,4,6,6,15,4,8,4,4,99]
-- *Lab6> testFs 10 2
-- [91,21,10,217,91,15,1729,15,9,2701]
-- *Lab6> testFs 10 3
-- [1247,15,91,561,2465,1729,1267,1045,1729,561]

-- Exercise 5: Use the carmichael method to further test Fermat. Time spent 1 hour.
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

testFCs :: Int -> Int -> IO [Integer]
testFCs n k = sequence $ take n stream
              where stream = (testF k carmichael) : stream

-- Test results:
-- As explained by the wiki page, these numbers most of the time bypass Fermat's test.
-- This is shown in the test results:
--
-- *Lab6> testFCs 10 1
-- [294409,294409,294409,294409,294409,294409,294409,294409,294409,294409]
-- *Lab6> testFCs 10 1
-- [294409,294409,294409,294409,294409,294409,294409,294409,294409,294409]
-- *Lab6> testFCs 10 1
-- [56052361,294409,294409,294409,294409,294409,294409,294409,294409,294409]
-- *Lab6> testFCs 10 1
-- [294409,294409,294409,294409,294409,294409,294409,294409,294409,294409]
-- *Lab6> testFCs 10 1
-- [294409,294409,294409,294409,294409,294409,294409,294409,294409,294409]
-- *Lab6> testFCs 10 1
-- [294409,294409,294409,294409,294409,294409,294409,294409,294409,294409]
-- *Lab6> testFCs 10 1
-- [294409,294409,294409,294409,294409,294409,294409,294409,294409,294409]
--
-- In some events however, the first number fails the test and a next number passes the test.

-- Exercise 6: Miller-Rabin primality check of carmichael numbers. Time spent 1 hour.
testMRs :: Int -> Int -> IO [Integer]
testMRs n k = sequence $ take n stream
              where stream = (testMR k carmichael) : stream

testMR :: Int -> [Integer] -> IO Integer
testMR k ns = testMR' k ns

testMR' :: Int -> [Integer] -> IO Integer
testMR' k (n:ns) = do b <- primeMR k n
                      if b
                      then return n
                      else testMR' k ns

-- Test results
-- The Miller-Rabin is able to figure out that those carmichael numbers are no prime. It however does fail rather fast for low k's. So applying k>4 would be advised to find some large numbers that slip through the test.
-- 
-- *Lab6> testMRs 10 1
-- [294409,56052361,173032371289,172947529,216821881,2301745249,27278026129,172947529,3711619793521,294409]
-- *Lab6> testMRs 10 2
-- [920153949774049,9203220800836849,14470947115561,959377262271049,8544361005001,3414146271409,1238966116844329,3296857440241,1339280649331561,9332984447209]
-- *Lab6> testMRs 1 3
-- [1746281192537521]
-- *Lab6> testMRs 1 4
-- [1225151403916168670761]

-- Exercise 7: Finding some large Mersenne primes. Time spent 2 hours
-- TODO, improve by removing lame show method to return a list of Mersenne primes.
mersenne k = mersenne' k primes
     where mersenne' k (p:ps) = do
              let m = 2^p - 1
              b <- primeMR k m
              when b (print ("M(" ++ show p ++ ")"))
              mersenne' k ps

-- *Lab6> mersenne 5
-- "M(2)"
-- "M(3)"
-- "M(5)"
-- "M(7)"
-- "M(13)"
-- "M(17)"
-- "M(19)"
-- "M(31)"
-- "M(61)"
-- "M(89)"
-- "M(107)"
-- "M(127)"
-- "M(521)"
-- "M(607)"
-- "M(1279)"
-- "M(2203)"
-- "M(2281)"
-- "M(3217)"
-- "M(4253)"
-- "M(4423)"
-- ^CInterrupted.

