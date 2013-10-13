module Lab6

where
import Data.List
import System.Random
import Week6

import Control.Monad

-- Exercise 1: Implement a fast modular exponentation.
-- This proved to be the fasted method (based on the Wikipedia algorithm "Right to left binary method"). Sadly this method was already given in Week6.hs
-- 
-- exM :: Integer -> Integer -> Integer -> Integer
-- exM _ 0 _ = 1
-- exM x y n = let 
--               z = exM x (y `div` 2) n
--               w = multM z z n
--             in 
--               if even y then w
--               else multM x w n

-- Exercise 2: Check efficiency of exM vs expM.
-- A normal computer is unable to store x^y (for large x and y) in memory. Therefor the algorithm relies on swapping of the computer. 
-- The other algorithm does not have to handle these large numbers, because it uses modular arithmetics to keep working with numbers of a maximum size of n - 1, where n is the modulo number.
--
-- To show the benefit of the other algorithm, we test the following cases:
modExpTest1a = expM m13 m14 5
modExpTest1b = exM m13 m14 5

-- In these cases expM will have trouble computing the large number, storing it and compute the modulo of it. Whilst the exM will return instant, because it divides the m14 to keep doing little computations.
-- In other test cases we saw "Interactive: Out of memory" on expM.

-- Exercise 3: Composites sieve.
composites :: [Integer]
composites = map fst (filter (not . snd) (sieve' [2..]))

-- characterize numbers by prime property
sieve' :: [Integer] -> [(Integer,Bool)]
sieve' ns = let ps = (sieve [2..])
            in map (\ n -> (n, (inlist n ps))) ns
            where inlist n (p:ps) | p < n     = inlist n ps
                                  | p == n    = True
                                  | otherwise = False

-- Exercise 4: Try to fool Fermat using composites.
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
-- [4,4,6,6,15,4,8,4,4,99] <-- min after 10 = 4
-- *Lab6> testFs 10 2
-- [91,21,10,217,91,15,1729,15,9,2701] <-- min after 10 = 9
-- *Lab6> testFs 10 3
-- [1247,15,91,561,2465,1729,1267,1045,1729,561] <-- min after 10 = 15
--
-- If we continue long enough we will hit 4 on every k (random (1,2) should return 1 k times in a row).

-- Exercise 5: Use carmichael numbers to trick Fermat's.
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
-- [294409,294409,294409,294409,294409,56052361,294409,294409,294409,294409]
-- *Lab6> testFCs 10 10
-- [56052361,294409,294409,56052361,294409,56052361,56052361,56052361,294409,294409]
-- *Lab6> testFCs 10 100
-- [118901521,118901521,228842209,118901521,118901521,56052361,56052361,118901521,228842209,56052361]
-- *Lab6> testFCs 10 1000
-- [65700513721,2301745249,65700513721,2301745249,11346205609,21515221081,13079177569,2301745249,9624742921,71171308081]
--
-- The Fermat test has to take a large k to not get fooled by the carmichael numbers. As can be shown in previous test results. 

-- Exercise 6: 
testMRs :: Int -> Int -> IO [Integer]
testMRs n k = sequence $ take n stream
              where stream = (testMR k carmichael) : stream

testMR :: Int -> [Integer] -> IO Integer
testMR k (n:ns) = do b <- primeMR k n
                     if b
                     then return n
                     else testMR k ns

-- Test results
-- The Miller-Rabin is able to figure out that those carmichael numbers are no prime. It however does fail rather fast for low k's. So applying k>4 would be advised to find some large numbers that slip through the test.
-- 
-- *Lab6> testMRs 10 1
-- [294409,56052361,173032371289,172947529,216821881,2301745249,27278026129,172947529,3711619793521,294409]
-- *Lab6> testMRs 10 2
-- [920153949774049,9203220800836849,14470947115561,959377262271049,8544361005001,3414146271409,1238966116844329,3296857440241,1339280649331561,9332984447209]
-- *Lab6> testMRs 10 3
-- [25061440015673569,156153292575625369,5690586528027001,6399336711287961361,2098397876980204801,18483957064801,173032371289,3680409480386689,235305452138976001,242979596944974611569]
-- *Lab6> testMRs 1 4
-- [1225151403916168670761]

-- Exercise 7: Try to find some mersenne primes. What did you find?
-- Tests whether 2^p-1 of prime p is also prime.
mersenne :: Integer -> IO Bool
mersenne 0 = error "p is not prime"
mersenne 1 = return True
mersenne p = do
           isp <- primeMR 1 p
           if isp then primeMR 1 (2^p-1)
           else error "p is not prime"

-- I have been naively pushing my machine to the max to find 2^m26-1, however, my hardware wasn't sufficient.
-- I managed to confirm the following:
testMersennes :: IO ()
testMersennes = tm' [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25] 1
    where tm' [] _     = print "Done"
          tm' (p:ps) i = do isprime <- primeMR 1 p
                            if isprime then do print ("OK: m" ++ show i)
                            else do print ("FAIL: m" ++ show i ++ " is NOT a prime!")
                            tm' ps (i+1)

-- Incredibly enough, k=1 seems to be reliable enough to show that mx is a mersenne prime. 
-- 
-- "OK: m22"
-- "OK: m23"
-- "OK: m24"
-- "OK: m25"
-- "Done"

-- Will list all Mersenne primes starting with number p.
findmersenne :: Integer -> IO ()
findmersenne p = findmersenne' (filter (>= p) primes)
    where findmersenne' (p:ps) = do
                            print ("Testing " ++ show p ++ "...")
                            ismersenne <- mersenne p
                            when ismersenne (print ("2^" ++ show p ++ "-1 is Mersenne prime!"))
                            findmersenne' ps

-- But is k=1 also enough to find all mersennes?
-- *Lab6> findmersenne 2
-- "2^2-1 is Mersenne prime!"
-- "2^3-1 is Mersenne prime!"
-- "2^5-1 is Mersenne prime!"
-- "2^7-1 is Mersenne prime!"
-- "2^13-1 is Mersenne prime!"
-- "2^17-1 is Mersenne prime!"
-- "2^19-1 is Mersenne prime!"
-- "2^31-1 is Mersenne prime!"
-- "2^61-1 is Mersenne prime!"
-- "2^89-1 is Mersenne prime!"
-- "2^107-1 is Mersenne prime!"
-- "2^127-1 is Mersenne prime!"
-- "2^521-1 is Mersenne prime!"
-- "2^607-1 is Mersenne prime!"
-- "2^1279-1 is Mersenne prime!"
-- "2^2203-1 is Mersenne prime!"
-- "2^2281-1 is Mersenne prime!
--
-- The MR-test with k=1 was able to find all mersenne primes up to m17 without unreliable results (in our short experiment at least). That is amazing. It however is not to be trusted entirely, because it still might yield a prime that is no prime.
--
-- After a while the findMersenne method revealed the 26th mersenne prime:
m26 = 2^23209-1
