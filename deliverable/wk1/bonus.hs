--exercise 1

foldrLength :: [a] -> Int
-- VVZ: arguments that you don't need, can be replaced with underscores
foldrLength = foldr (\x y -> 1 + y) 0

foldrElem :: (Eq a) => [a] -> a -> Bool
foldrElem xs x = foldr (\y z -> z || x == y) False xs

foldrOr = foldr (||) False

foldrMap :: (a -> b) -> [a] -> [b]
-- VVZ: could have dropped the xs at the end - cf. http://www.haskell.org/haskellwiki/Eta_conversion
-- VVZ: another cool thing: a colon is also a function in Haskell, so you could have written:
-- VVZ: foldrMap f = foldr ((:) . f) []
foldrMap f xs = foldr (\y ys -> (f y) : ys) [] xs

filterItem :: (a -> Bool) -> a -> [a] -> [a]
filterItem p x xs | p x       = x : xs
                  | otherwise = xs
				  
foldrFilter :: (a -> Bool) -> [a] -> [a]
foldrFilter p xs = foldr (\y ys -> (filterItem p y ys)) [] xs

foldrConcat :: [a] -> [a] -> [a]
-- VVZ: (++) is also a function, so (\y ys -> ys ++ y) is the same as (\y -> (++y))
foldrConcat xs ys = foldr (\y ys -> ys ++ y) [] [xs, ys]

-- VVZ: however, you cheat a bit here by still using (++). What we meant was like this:
foldrConcat' :: [a] -> [a] -> [a]
foldrConcat' [] ys = ys
foldrConcat' xs ys = foldrConcat' (init xs) (last xs : ys)

foldrReverse :: [a] -> [a]
-- VVZ: you could have used your foldrConcat instead of (++) ;)
foldrReverse xs = foldr (\y ys -> ys ++ [y]) [] xs

--exercise 2
foldlReverse = foldl (flip (:)) [] 

--exercise 3
--http://stackoverflow.com/questions/3082324/foldl-versus-foldr-behavior-with-infinite-lists
--foldr is lazy, foldl cannot be due to its backwards nature, so we cannot use foldl to operate on infite lists.

--exercise 4
data Creature = Lady | Tiger
	deriving (Eq,Show)

sign1, sign2 :: (Creature,Creature) -> Bool
sign1 (x,y) = x == Lady || y == Lady
sign2 (x,y) = x == Tiger

solution2 :: [(Creature,Creature)]
solution2 = [ (x,y) | x <- [Lady,Tiger], y <- [Lady,Tiger], sign1 (x,y) == sign2 (x,y) ]

--exercise 5
data Islander = Knight | Knave
	deriving (Eq,Show)

john, bill :: (Islander,Islander) -> Bool
john (x,y) = (x == y)
bill (x,y) = (x /= y)

solution3 = [(x,y) | x <- [Knight,Knave], 
                     y <- [Knight,Knave], 
                     john (x,y) == (x == Knight) && bill(x,y) == (y == Knight)]
--In this puzzle, again John is on the left, Bill on the right. John says: “We are both
--of the same kind.” Bill says: “We are both of different kinds.” Who is what?

--exercise 6
--solution: (Lady,Tiger)

-- signs are both correct or both incorrect
-- sign 1 says: if this room has a tiger, the other room has a lady
-- sign 2 says: this room has a tiger
solution6 :: [(Creature,Creature)]
sign1a (x,y) = (x == Tiger) <= (y == Lady)
sign2a (x,y) = y == Tiger
solution6 = [ (x,y) | x <- [Lady,Tiger], y <- [Lady,Tiger], sign1a (x,y) == sign2a (x,y) ]

--exercise 7

data Boy = Matthew | Peter | Jack | Arnold | Carl
		deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]
matthew, peter, jack, arnold, carl :: Boy -> Bool
matthew = \ x -> not (x==Matthew) && not (x==Carl)
peter = \ x -> x==Matthew || x==Jack
jack = \ x -> not (matthew x) && not (peter x)
arnold = \ x -> matthew x /= peter x
carl = \ x -> not (arnold x)
declarations = [matthew,peter,jack,arnold,carl]
table = zip declarations boys

tf = [True, False]

permutations = filter (\x -> length (filter (\y -> y) x) == 2) [[v,w,x,y,z] | v <- tf, w <- tf, x <- tf, y <- tf, z <- tf]

checkStatements :: [(Bool, (Boy -> Bool), Boy)] -> Bool
checkStatements xs = and (map (\(liar, p, b) -> (checkStatement liar (p b))) xs)

checkStatement :: Bool -> Bool -> Bool
checkStatement liar value | liar    = not value
                          | otherwise = value

permutations2 :: [([Bool],Bool, Boy)]
permutations2 =	[(liarSetup, checkStatements (zip3 liarSetup declarations (take 5 (repeat target))), target) | liarSetup <- permutations, target <- boys]

solved = map (\(x,_,y) -> (x, y)) (filter (\(_,x,_) -> x) permutations2)

fst3 (x,y,z) = x
lst3 (x,y,z) = z

honest = (map snd (filter (\x -> not (fst x)) (zip (fst (head solved)) boys)))

solution = snd (head solved)

--Now write a function solution that lists the boys that could have
--done it, and a function honest that lists the boys that have made
-- honest declarations, for each member of the solution list.

