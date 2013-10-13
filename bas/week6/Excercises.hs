module Excercises
where

import Week6

              
composites :: [Integer]
composites = composites' primes2 [1..]
        where 
            composites' (b:bs) (n:ns) | b         = composites' bs ns
                                      | otherwise = n : composites' bs ns
                          
primes2 = (True : True : primes2' [3..] (\x -> rem x 2 /= 0))
        where primes2' (n:ns) f = (f n) : primes2' ns (\x -> (f x) && ((rem x n) /= 0))
        
        
--exercise 4
--k = 1, lowest outcome = 4
--k = 2, still able to get 4, generally much larger though
--k = 3, also able to get 4
--k = 4, got a 9
--as k increases, so does the running time (drastically)
firstComposite k = failsFermat k 0
                   where failsFermat k n = do 
                                           let v = composites !! n
                                           p <- primeF k v
                                           if p then return (k, v)
                                           else failsFermat k (n + 1)
                                           
                                           
--exercise 5
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

--  mapM (primeF 10) (take 10 carmichael)
-- [True,True,True,True,True,True,True,True,True,True]
--  mapM (primeF 100) (take 10 carmichael)
-- [False,False,True,True,True,True,True,True,True,True]
--  mapM (primeF 1000) (take 10 carmichael)
-- [False,False,False,False,False,False,False,False,True,True]
--The numbers fool Fermat's primality check and require
--a relatively larger k to be reliable.


--exercise 6
-- mapM (primeMR 1) (take 10 carmichael)
-- [False,False,False,False,False,False,False,False,False,False]
-- the test breaks carmichael numbers, but still requires a sufficiently large k:
-- filterM (primeMR 1) (take 1000 composites)
-- [6,9,46,49,81,91,175,341,793,931]


--excercise 7
mersenne k n = do
              p <- primeMR k n
              if not p then return False
              else primeMR k (2^n-1)
              
mersennes = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25]

findNextMersenne p = do 
                     print $ show p
                     m <- mersenne 1 p
                     if m then print "found mersenne"
                     else findNextMersenne (p + 1)

                     
                     
                   