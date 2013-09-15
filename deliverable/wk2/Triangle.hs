module Triangle
where

import Data.List
import Uncurry

data Shape = NoTriangle         -- sum of two sides lesser or equal than the third
           | Equilateral        -- all sides of equal length
           | Isosceles          -- two sides of equal length
           | Rectangular        -- Pythagorean (a^2 + b^2 = c^2)
           | Other              -- the former rules do not apply
             deriving (Eq,Show)

-- determine the triangle type from the length of three sides a,b,c
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangle' (sort [a,b,c])
           where triangle' [a,b,c] | a + b <= c             = NoTriangle
                                   | (a == b) && (a == c)   = Equilateral
                                   | (a == b) || (b == c)   = Isosceles
                                   | (a^2) + (b^2) == (c^2) = Rectangular
                                   | otherwise              = Other

-- Test cases
--
-- The test cases listed here are set up to test the (formal) definition of the
-- triangle types. The triangle types have properties as listed beside them in
-- the declaration of these types.
-- The properties are used to create 3-tuples that satisfy these properties.
-- Then these 3-tuples are passed to the test function to assert that it holds.

equilaterals = [(1,1,1),(5,5,5),(1024,1024,1024)]
rectangulars = [(3,4,5),(5,12,13),(8,15,17),(7,24,25),(68,285,293)]
isosceles    = [(3,3,4),(3,4,3),(4,3,3),(100,1,100)]
notriangles  = [(2,2,4),(283,495,1000),(1,2,3),(6,2,1)]
others       = [(19,12,18),(2^63,2^7,2^63+1)]

-- combined list of all test cases
allTests = [(equilaterals,Equilateral),
            (isosceles,Isosceles),
            (rectangulars,Rectangular),
            (notriangles,NoTriangle),
            (others,Other)]

-- test function
test :: [(Integer,Integer,Integer)] -> Shape -> Bool
test shapes expected = and (map (\ item -> (uncurry3 triangle item) == expected) shapes)

-- run test suite
runTests :: [(Shape,Bool)]
runTests = map (\ (testCase,expected) -> (expected, (test testCase expected))) allTests
