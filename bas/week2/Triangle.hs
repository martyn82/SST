module Triangle

where
import Uncurry3
import Data.List

data TriangleType = NotATriangle | Equilateral | Rectangular | Isosceles | Other
	 deriving (Eq, Show)
	 
triangleType :: Integer -> Integer -> Integer -> TriangleType
triangleType a b c = typeFromSorted (sort [a,b,c])
				   
--Precondition: a <= b <= c
typeFromSorted [a, b, c] | or (map (<1) sides) || a + b <= c = NotATriangle -- Inequality rule or zero/negative values.
					     | a == b && a == c                  = Equilateral  -- All sides are the same, equilateral triangle
                         | a == b || b == c     		     = Isosceles    -- There is a duplicate length -> Isosceles
					     | (a^2) + (b^2) == (c^2) 		     = Rectangular  -- Pythagoras is valid -> Rectangular
					     | otherwise 				  	     = Other        -- No match, other type of triangle
					     where sides = [a, b, c];
				   
				   
--combinations of negative and/or zero values
notATriangleTests = [(0,1,1), (1,0,1), (1,1,0), (-1,1,1), 
                     (1,-1, 1), (1,1,-1), (0,0,0), (-1,-1,-1), (-1,-1, 1),
					 --combinations that cannot be a triangle due to inequality
					 (2,2,5), (2,5,2), (5,2,2), (2,2,4), (1,2,1), (2,1,1)] 
					 
--any positive integer repeated 3 times				 
equilateralTests  = [(1,1,1), (3,3,3), (5,5,5)] 

 --two of the same length in different orders
isoscelesTests    = [(2,2,3), (3,5,5), (5,2,5)]

--pythagorean triples from wikipedia
rectangularTests  = [(3,4,5), (85, 132, 157), (119, 120, 169), (52, 165, 173)] 

--slightly altered triples to not be pythagorean anymore, and don't match any other cases
otherTests = [(3,6,5), (84, 133, 157), (119, 110, 169), (53, 165, 173)] 


--run set of tuples against triangle testing
runTests :: [(Integer, Integer, Integer)] -> TriangleType -> Bool
runTests tests expected = and (map (\item -> (uncurry3 triangleType item) == expected) tests)


--pair the tests with their expected values
allTests = [(notATriangleTests, NotATriangle), 
			(equilateralTests, Equilateral), 
			(isoscelesTests, Isosceles), 
			(rectangularTests, Rectangular),
			(otherTests, Other)]
			
--run all tests
runAllTests :: [(TriangleType, Bool)]
runAllTests = map (\(test, expected) -> (expected, (runTests test expected))) allTests


