module GS
where

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
	| k^2 > n     = n		-- Ex.1.4 it does not make any difference whether to specify > or >= because if k^2 == n, then k divides n
	| otherwise   = ldf (k+1) n

prime0 :: Integer -> Bool
prime0 n | n < 1	= error "not a positive integer"
	 | n == 1	= False
	 | otherwise	= ld n == n

-- Ex.1.6 type declaration of rem :: Integer -> Integer -> Integer
-- Ex.1.7 :t divides 5 :: Integer -> Bool ; :t divides 5 7 :: Bool ; probably because we've substituted variables for constants


-- GS.hs from book website

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

min' :: Int -> Int -> Int 
min' x y | x <= y    = x
         | otherwise = y

average :: [Int] -> Rational
average [] = error "empty list" 
average xs = toRational (sum xs) / toRational (length xs)

sum' :: [Int] -> Int
sum' [] = 0 
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0 
length' (x:xs) = 1 + length' xs

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys 

factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ld n

primes0 :: [Integer]
primes0 = filter prime0 [2..]    

ldp :: Integer -> Integer
ldp n = ldpf primes1 n

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p 
              | p^2 > n      = n
              | otherwise    = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False 
        | otherwise = ldp n == n

a = 3
b = 4 
f :: Integer -> Integer -> Integer
f x y = x^2 + y^2

g :: Integer -> Integer 
g 0     = 0
g x     = 2 * (g (x+1)) 

h1 :: Integer -> Integer 
h1 0 = 0
h1 x = 2 * (h1 x) 

h2 :: Integer -> Integer 
h2 0 = 0
h2 x = h2 (x+1) 

-- Exercises continued

-- Ex.1.9 Define function for getting maximum of a list of integers
maxInt :: [Int] -> Int
maxInt [] 	= error "empty list"
maxInt [x] 	= x
maxInt (x:xs) 	= max x (maxInt xs)

-- Ex.1.10 removeFst removes the first occurrence of an integer m from a list of integers.
removeFst :: Int -> [Int] -> [Int]
removeFst x []	   = []
removeFst x (y:ys) | x == y	 = ys
		   | otherwise   = y : removeFst x ys


