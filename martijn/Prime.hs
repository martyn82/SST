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

