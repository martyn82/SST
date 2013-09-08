divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld n = ldf 2 n
ldf k n | divides k n = k
        | k ^ 2 > n   = n
		| otherwise   = ldf (k + 1) n

prime0 n | n < 1 = error "Not a positive integer"
         | n == 1 = False
    	 | otherwise = ld n == n
		 
		 
--Excercise 1.9

maxInList :: [Int] -> Int
maxInList [ ] = error "Empty list"
maxInList [x] = x
maxInList (x:rest) = max x (maxInList rest)

--Exercise 1.10
removeFst :: Integral a => a -> [a] -> [a]
removeFst  a [] = []
removeFst  a (b:rest 	) | a == b = rest
                      | otherwise = b : (removeFst a rest)
					  
					  
--Exercise 1.11
--Helper function (don't have GS module)
minInList :: [Int] -> Int
minInList [ ] = error "Empty list"
minInList [x] = x
minInList (x:rest) = min x (minInList rest)

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = minInList xs


--1.12
avgInList :: [Integer] -> Rational
avgInList [] = error "Empty list"
avgInList list = toRational (sum list) / toRational (length list)


--1.13
myCount :: Char -> String -> Int
myCount c str = length (filter (==c) str)

--1.14
blowupHelper :: String -> Int -> String
blowupHelper [] _ = []
blowupHelper (c:str) i = (replicate i c) ++ (blowupHelper str (i + 1))

blowup :: String -> String
blowup str = blowupHelper str 1

--1.15 (using quicksort)
srtString :: [String] -> [String]
srtString (pivot:list) = (srtString smaller) ++ [pivot] ++ (srtString bigger)
						 where smaller = filter (< pivot) list; bigger = filter (>= pivot) list 
						 --what is the correct syntax with multiple wheres??
srtString [] = []

--1.16
prefix :: String -> String -> Bool
prefix [] ys = True
prefix xs [] = False
prefix (x:xs) (y:ys) | (length xs) > (length ys) = False
                     | otherwise = (x == y) && prefix xs ys
					 
					 
--1.17
substring :: String -> String -> Bool
substring xs [] = False
substring (xs) (y:ys) | (prefix xs (y:ys)) = True
                      | otherwise = (substring xs ys)

-- 1.18
-- 1.	["Alice", "Bob"] :: [[Char]]
-- 2.	(True, "Banana") :: (Bool, [Char])
-- 3.	[(True, "Banana"), (False, "Apple")] :: [(Bool, [Char])]
-- 4.	([True, False, True], "Banana") :: ([Bool], [Char])
-- 5.	(not) :: Bool -> Bool

-- 1.19
-- 1.	head :: [a] -> a
-- takes the first element out of a list: head [5..10]
-- 2.	last :: [a] -> a 
-- takes the last element out of a list: last [5..10]
-- 3.	init :: [a] -> [a]
-- removes the last element of of the list: init [5..10]
-- 4.	fst :: (a, b) -> a
-- takes the first element from a tuple: fst (2,6,4)
-- 5.	(++) :: [a] -> [a] -> [a]
-- concatenation: [1..2] ++ [5..7]
-- 6.	flip :: (a -> b -> c) -> b -> a -> c
-- switch two input parameters: flip (substring) "Bobb" "Bob"
-- 7.	flip (++) :: [a] -> [a] -> [a]
-- switch two input parameters: flip (++) [1..2] [5..7]

--1.20
lengths :: [[a]] -> [Int]
lengths x = map length x

--1.21
sumlengths :: [[a]] -> Int
sumlengths x = sum (lengths x)

--1.23
ldp :: Integer -> Integer
ldp = ldpf primes1 

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
		
--1.24
--ldp's signature does not alter; the parameter entered will be passed through to ldpf implicitly.

					 
					 


