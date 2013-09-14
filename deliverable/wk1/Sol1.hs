module Sol1 where

import GS

-- 1.9
mxmInt :: [Int] -> Int
mxmInt [] = error "empty list"
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

-- 1.10
removeFst :: Int -> [Int] -> [Int]
removeFst x []	   = []
removeFst x (y:ys) | x == y	 = ys
		   | otherwise   = y : removeFst x ys

-- 1.13
count :: Char -> String -> Int
count a []     = 0
count a (x:xs) | a == x    = 1 + (count a xs)
               | otherwise = (count a xs)

-- 1.14
blowupHelper :: String -> Int -> String
blowupHelper [] _ = []
blowupHelper (c:str) i = (replicate i c) ++ (blowupHelper str (i + 1))

blowup :: String -> String
blowup str = blowupHelper str 1

-- 1.15
srtString :: [String] -> [String]
srtString []     = []
srtString (x:xs) = (srtString lt) ++ [x] ++ (srtString leq)
	where lt = filter (<x) xs
	      leq = filter (>= x) xs

-- 1.17
substring :: String -> String -> Bool
susbstring [] ys = False
substring xs [] = False
substring xs ys | prefix xs ys = True
                | otherwise = substring xs (tail ys)

-- 1.20
lengths :: [[a]] -> [Int]
lengths xss = map length xss

-- 1.21
sumLengths :: [[a]] -> Int
sumLengths xss = sum (lengths xss)
