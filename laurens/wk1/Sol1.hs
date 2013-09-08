module Sol1 where

import GS

-- 1.9
mxmInt :: [Int] -> Int
mxmInt [] = error "empty list"
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

-- 1.10
removeFst :: Int -> [Int] -> [Int]
removeFst n [] = []
removeFst n (x:xs) | x == n    = xs
                   | otherwise = x : (removeFst n xs)

removeFstString :: String -> [String] -> [String]
removeFstString s [] = []
removeFstString s (x:xs) | x == s = xs
                         | otherwise = x : (removeFstString s xs)

-- 1.11
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let m = mnmInt xs
              in m : (srtInts' (removeFst m xs))

-- 1.13
count :: Char -> String -> Int
count a []     = 0
count a (x:xs) | a == x    = 1 + (count a xs)
               | otherwise = (count a xs)

-- 1.14
blowup :: String -> String
blowup [] = []
blowup xs = blowup' (zip xs [1..])

blowup' :: [(Char, Int)] -> String
blowup' []          = []
blowup' ((c, n):cs) = replicate n c ++ blowup' cs

-- 1.15
srtString :: [String] -> [String]
srtString []     = []
srtString (x:xs) = srtString (filter (<x) xs) ++ [x] ++ srtString (filter (>= x) xs)

-- 1.17
substring :: String -> String -> Bool
susbstring [] ys = False
substring xs [] = False
substring xs ys | prefix xs ys || substring xs (tail ys) = True
                | otherwise = False

-- 1.20
lengths :: [[a]] -> [Int]
lengths xss = map length xss

-- 1.21
sumLengths :: [[a]] -> Int
sumLengths xss = sum (lengths xss)
