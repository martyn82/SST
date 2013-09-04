module Prime
where

-- Ex.1.4 it does not make any difference whether to specify > or >= because if k^2 == n, then k divides n
-- Ex.1.6 type declaration of rem :: Integer -> Integer -> Integer
-- Ex.1.7 :t divides 5 :: Integer -> Bool ; :t divides 5 7 :: Bool ; probably because we've substituted variables for constants

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

-- Ex.1.13 Counting number of occurrences of character within a string.
count :: Char -> String -> Int
count x []     = 0
count x (y:ys) | x == y		= 1 + count x ys
	       | otherwise	= 0 + count x ys

-- Ex.1.14 Blowup
blowup :: String -> String
blowup xs = blowup' xs 1
	where blowup' [] _     = []
              blowup' (x:xs) n =  take n (repeat x) ++ blowup' xs (n+1)

-- Ex.1.15 Sort a list of strings alphabetically
srtString :: [String] -> [String]
srtString []      = []
srtString (x:xs)  = srtString small ++ (x : srtString large)
	where small = [y | y <- xs, y <= x]
              large = [y | y <- xs, y > x]

-- Ex.1.17 Substring determines whether a string is a substring of another
prefix :: String -> String -> Bool
prefix [] ys         = True
prefix (x:xs) []     = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

substring :: String -> String -> Bool
substring xs [] = False
substring xs (y:ys) = prefix xs (y:ys) || substring xs ys

-- Ex.1.18 Typed expressions
-- :t ["a","b"] :: [String]
-- :t (True,"a") :: (Bool,String)
-- :t [(True,"a")] :: [(Bool,String)]
-- :t ([True],"a") :: ([Bool],String)
-- :t \True -> False :: Bool -> Bool

-- Ex.1.19 Find types (type of)
-- :t head :: [a] -> a
-- :t last :: [a] -> a
-- :t init :: [a] -> [a]
-- :t fst  :: (a,b) -> a
-- :t (++) :: [a] -> [a] -> [a]
-- :t flip :: (a -> b -> c) -> b -> a -> c
-- :t flip(++) :: [a] -> [a] -> [a]

-- head [1,2,3] = 1
-- last [1,2,3] = 3
-- init [1,2,3] = [1,2]
-- fst  (1,2) = 1
-- (++) [1,2] [3] = [1,2,3]
-- flip (>) 2 3 = True // flip (<) 2 3 = False
-- flip (++) [2] [3] = [3,2]


