module Lab6

where
import Data.List
import System.Random
import Week6

import Data.Bits

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

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
