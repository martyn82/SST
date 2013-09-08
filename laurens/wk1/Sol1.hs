module Sol1 where

import GS

mxmInt :: [Int] -> Int
mxmInt [] = error "empty list"
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

removeFst :: Integral => a -> [a] -> [a]
removeFst n [] = []
removeFst n (x:xs) | x == n    = xs
                   | otherwise = x : (removeFst n xs)

removeFstString :: String -> [String] -> [a]
removeFstString s [] = []
removeFstString s (x:xs) | x == s = xs
                         | otherwise = x : (removeFstString s xs)

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let m = mnmInt xs
              in m : (srtInts' (removeFst m xs))

count :: Char -> String -> Int
count a []     = 0
count a (x:xs) | a == x    = 1 + (count a xs)
               | otherwise = (count a xs)

blowup :: String -> String
blowup [] = []
blowup xs = blowup' (zip xs [1..])

blowup' :: [(Char, Int)] -> String
blowup' []          = []
blowup' ((c, n):cs) = replicate n c ++ blowup' cs

mnmString :: [String] -> String
mnmString [] = error "empty list"
mnmString [x] = x
mnmString (x:xs) = min x (mnmString xs)

srtString :: [String] -> [String]
srtString []     = []
srtString xs = let x = mnmString xs
               in x : (srtString (removeFstString xs))
