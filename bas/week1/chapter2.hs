import TalkingAboutMathematicalObjects;
--Helper functions to print truth tables

t = True
f = False

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 fn =  \(a,b,c) -> fn a b c


cartProd2 xs ys = [(x,y) | x <- xs, y <- ys]
cartProd3 xs ys zs = [(x,y,z) | x <- xs, y <- ys, z <- zs]

allBooleans2 = cartProd2 [t, f] [t, f]
allBooleans3 = cartProd3 [t, f] [t, f] [t, f]

truthtable :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
truthtable bf = zip3 (map fst allBooleans2) (map snd allBooleans2) (map (uncurry bf) allBooleans2)

toTFString :: Bool -> String
toTFString True  = "T"
toTFString False = "F"

toString :: (Bool, Bool, Bool) ->  String
toString (p,q,r) = (toTFString p) ++ " | " ++ (toTFString q) ++ " | " ++ (toTFString r)


printtable bf = mapM_ putStrLn ("P | Q | R" : map toString(truthtable bf))



--2.2 XOR
-- P  Q  | P XOR Q
-- T  T  |    F
-- T  F  |    T
-- F  T  |    T
-- T  T  |    F

--2.4
--Correct, it matches the negation of the XOR table.

--2.9
-- printtable (\p q -> (p <+> q) <+> q)
-- P | Q | R
-- T | T | T
-- T | F | T
-- F | T | F
-- F | F | F

--2.13
validateEquality :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
validateEquality bf1 bf2 = and (map (\p -> (bf1 p) == (bf2 p)) [t, f])
validateEquality2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
validateEquality2 bf1 bf2 = and (map (\(p, q) -> (bf1 p q) == (bf2 p q)) allBooleans2)
validateEquality3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
validateEquality3 bf1 bf2 = and (map (\(p, q, r) -> (bf1 p q r) == (bf2 p q r)) allBooleans3)

test2121  = (not t == f) && (not f == t)
test2122  = validateEquality (\p -> p ==> f) (\p -> not p)
test2123a = validateEquality (\p -> p || t) (\p -> t)
test2123b = validateEquality (\p -> p && f) (\p -> f)
test2124a = validateEquality (\p -> p || f) (\p -> p)
test2124b = validateEquality (\p -> p && t) (\p -> p)
test2125  = validateEquality (\p -> p || not p) (\p -> t)
test2126  = validateEquality (\p -> p && not p) (\p -> f)

test212 = and [test2121, test2122, test2123a, test2123b, test2124a, test2124b, test2125, test2126]

--2.15
isContradiction1 :: (Bool -> Bool) -> Bool
isContradiction1 bf = not (or (map bf [t,f]))

isContradiction2 :: (Bool -> Bool -> Bool) -> Bool
isContradiction2 bf = not (or (map (uncurry bf) allBooleans2))

isContradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
isContradiction3 bf = not (or (map (uncurry3 bf) allBooleans3))

--2.16
--1.	There is no solution for the equation x² + 1 = 0
--2.	A largest natural number exists.
--3.	The number 13 is not a prime.
--4.	The number n is not a prime.
--4.	The number of primes is finite.

--2.17
-- x < y > z

--2.18
-- 1.	Using truth table equality, changing all
-- 		values to their opposite still yields the
-- 		same truth table, when using the 'double implication'
-- 2.	if (not Φ) <=> Ψ is logically valid, we can say that (Ψ <=> (not Φ))
--2.19
-- if Ψ <=> Φ then Ψ ≡ Φ because they yield the same output for any input given. 
-- therefore, Φ can be replaced by Ψ and vice versa.


--2.20
test2201 = validateEquality2 (\p q   -> not p ==> q)      (\p q   -> p ==> not q)
test2202 = validateEquality2 (\p q   -> not p ==> q)      (\p q   -> q ==> not p)
test2203 = validateEquality2 (\p q   -> not p ==> q)      (\p q   -> not q ==> p)
test2204 = validateEquality3 (\p q r -> p ==> (q ==> r))  (\p q r -> q ==> (p ==> r))
test2205 = validateEquality3 (\p q r -> p ==> (q ==> r))  (\p q r -> (p ==> q) ==> r)
test2206 = validateEquality2 (\p q   -> (p ==> q) ==> p)  (\p q   -> p)
test2207 = validateEquality3 (\p q r -> p || q ==> r)     (\p q r -> (p ==> r) && (q ==> r))

test220 = zip [1..7] [test2201, test2202, test2203, test2204, test2205, test2206, test2207]

--2.21
--1.	p => q
-- 	 	printtable (\p q -> q ==> p)
--2.	2^2^2 = 16

--3.	true, false, p, q, p and q, q and p, p => q, q => p, etc etc
--4.	'Brute force' method:
--		find all permutations of [(p and q), (p and not q), (not p and q), (not p and not q)]
--		join them using a conjunction (binary OR). Each item in the list represents an outcome
--      where a single value is true.

--5.	answer for more variables: nFormulas = 2 ^ (2 ^ n), answer 4 still applies, just more permutations will appear.

--2.22
--the number of decimals that can be used in a rational number
--is unlimited. Therefore, if we have 3/10 and 4/10 we can generate 
--35/100, between 1/1000 and 2/1000 we have 15/10000. This can 
--be repeated infinitely, therefore the statement is true.

--2.23
-- 1.
--	∀x(Ax => (Bx => Cx))
--          |
--     Ax => (Bx => Cx)
--          |
--     		  Bx => Cx
-- 2.
-- Ǝx(Ax ∧ Bx)
--       | 
--    Ax ∧ Bx
--    |     | 
--    Ax   Bx
-- 3.
-- ƎxAx ∧ ƎxBx
--   |      |
-- ƎxAx   ƎxBx
--   |      |
--  Ax      Bx

--2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1


--2.52
parity :: [Bool] -> Bool
parity xs = mod (length (filter (\x -> x) xs)) 2 == 0

--2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)








