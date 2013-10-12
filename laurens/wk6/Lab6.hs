module Lab6

where
import Data.List
import System.Random
import Week6

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
