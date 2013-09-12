module Lab2 where

import Data.List
import Week2

-- Triangle exercise.
data Shape = NoTriangle
           | Equilateral
           | Isosceles
           | Rectangular
           | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangle' (sort [a, b, c])

-- [Integer] must be sorted from low to high and contain 3 elements.
triangle' :: [Integer] -> Shape
triangle' [a, b, c] | a > 0 && a + b <= c = NoTriangle
                    | a == b && b == c = Equilateral
                    | (a^2) + (b^2) == (c^2) = Rectangular
                    | (a == b && b /= c) || (b == c && c /= a) || (a == c && c /= b) = Isosceles
                    | otherwise = Other

-- satisfiability, tautology, contradiction..
-- satisfiable :: Form -> Bool
-- satisfiable f = any (\ v -> eval v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\x -> eval x f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

entails2 :: Form -> Form -> Bool
entails2 f g = contradiction (Cnj [f, (Neg g)])

equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

