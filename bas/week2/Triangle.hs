module Triangle

where
import Uncurry3
import Data.List

data TriangleType = NotATriangle | Equilateral | Rectangular | Isosceles | Other
	 deriving (Eq, Show)
	 
triangleType :: Integer -> Integer -> Integer -> TriangleType
triangleType a b c = triangleType' (sort [a,b,c])
				   
--Precondition: a <= b <= c
triangleType' [a, b, c] | a < 1 || a + b <= c 	  = NotATriangle 
						-- pre: a <= b <= c
						-- if a >= 1 then b >= 1 and c >=  1
						-- if a + b > c then b + c > a and a + c > b
						| a == b && a == c       = Equilateral
						--if a == b && b == c then a == c
						| a == b || b == c       = Isosceles    
						--we know: a <= b <= c; a,b,c are not all equals
						--then we can say that (a /= b) => (a /= c), so we don't need to test (a /= c)
						| (a^2) + (b^2) == (c^2) = Rectangular  
						-- Pythagoras is valid -> Rectangular
						| otherwise 			  = Other
						-- No match, other type of triangle
						where sides = [a, b, c];

-- Testing strategy: I'm not sure how to proceed: take 
-- the equilateral check. We could prove it for all N
-- if we look at the code: (a == b && a == c) == (a + 1 == b + 1 && a + 1 == c + 1)
-- however, this depends on knowing the implementation. If this is not a problem
-- then we can use (1,1,1) as a test case for Equilateral and (1,2,2) for Isosceles.
-- I would have no clue how to prove pythagoras; though it makes sense as long as a² + b² = c².					
				   
--combinations of negative and/or zero values
notATriangleTests = [(0,1,1), (1,0,1), (1,1,0), (-1,1,1), 
                     (1,-1, 1), (1,1,-1), (0,0,0), (-1,-1,-1), (-1,-1, 1),
					 --combinations that cannot be a triangle due to inequality
					 (2,2,5), (2,5,2), (5,2,2), (2,2,4), (1,2,1), (2,1,1)] 
					 
--any positive integer repeated 3 times				 
equilateralTests  = [(1,1,1), (3,3,3), (5,5,5)] 

 --two of the same length 
isoscelesTests    = [(2,2,3), (3,5,5), (5,2,5)]

--pythagorean triples
pythagoreanTriples = pythagoreanTriples' (3,4,5)
pythagoreanTriples' (a,b,c) = (a,b,c) : (pythagoreanTriples' (a*2,b*2, c*2))

rectangularTests  =  take 30 pythagoreanTriples

--slightly altered triples to not be pythagorean anymore, and won't match any other cases
otherTests = map (\(a,b,c) -> (a,b,c +1)) rectangularTests


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


