--exercise 1

foldrLength :: [a] -> Int
foldrLength = foldr (\x y -> 1 + y) 0

foldrElem :: (Eq a) => [a] -> a -> Bool
foldrElem xs x = foldr (\y z -> z || x == y) False xs

foldrOr = foldr (||) False

foldrMap :: (a -> b) -> [a] -> [b]
foldrMap f xs = foldr (\y ys -> (f y) : ys) [] xs

filterItem :: (a -> Bool) -> a -> [a] -> [a]
filterItem p x xs | p x       = x : xs
                  | otherwise = xs
				  
foldrFilter :: (a -> Bool) -> [a] -> [a]
foldrFilter p xs = foldr (\y ys -> (filterItem p y ys)) [] xs

foldrConcat :: [a] -> [a] -> [a]
foldrConcat xs ys = foldr (\y ys -> ys ++ y) [] [xs, ys]

foldrReverse :: [a] -> [a]
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
sign2 (x,y) = y == Tiger

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
