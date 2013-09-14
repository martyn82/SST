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

-- CNF Converter
toCNF :: Form -> Form
toCNF = cnf . nnf . arrowfree

-- precondition: input is arrow-free and in NNF
cnf :: Form -> Form 
cnf (Prop x) = Prop x
cnf (Neg x) = Neg x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (f:fs)) = foldl dist (cnf f) (map cnf fs)

-- precondition: input is CNF
dist :: Form -> Form -> Form
dist (Cnj fs) gs = Cnj [dist f gs | f <- fs]
dist fs (Cnj gs) = Cnj [dist fs g | g <- gs]
dist fs gs = Dsj [fs, gs]

-- To CNF tests:
-- af - ArrowFree, f = formula, r = result (expected)
af_f_1 = p
af_r_1 = p

af_f_2 = (Neg af_f_1)
af_r_2 = (Neg af_r_1)

af_f_3 = (Cnj [af_f_1, af_f_2])
af_r_3 = (Cnj [af_r_1, af_r_2])

af_f_4 = (Dsj [af_f_3, af_f_1])
af_r_4 = (Dsj [af_r_3, af_r_1])

af_f_5 = (Impl (Prop 1) (Prop 2))
af_r_5 = (Dsj [(Neg (Prop 1)), (Prop 2)])

af_f_6 = (Equiv (Prop 1) (Prop 2))
af_r_6 = (Cnj [(Dsj [(Neg (Prop 1)), (Prop 2)]), (Dsj [(Prop 1), (Neg (Prop 2))])])

af_check_1 = (arrowfree af_f_1) == af_r_1
af_check_2 = (arrowfree af_f_2) == af_r_2
af_check_3 = (arrowfree af_f_3) == af_r_3
af_check_4 = (arrowfree af_f_4) == af_r_4
af_check_5 = (arrowfree af_f_5) == af_r_5
af_check_6 = (arrowfree af_f_6) == af_r_6
af_check_7 = (arrowfree' af_f_6) == af_r_6

-- NNF tests
nnf_f_1 = p
nnf_r_1 = p

nnf_f_2 = (Neg nnf_f_1)
nnf_r_2 = (Neg nnf_r_1)

nnf_f_3 = (Neg nnf_f_2)
nnf_r_3 = nnf_r_1

nnf_f_4 = (Cnj [nnf_f_1, nnf_f_2, nnf_f_3])
nnf_r_4 = (Cnj [nnf_r_1, nnf_r_2, nnf_r_3])

nnf_f_5 = (Dsj [nnf_f_1, nnf_f_2, nnf_f_3])
nnf_r_5 = (Dsj [nnf_r_1, nnf_r_2, nnf_r_3])

nnf_f_6 = (Neg nnf_f_4)
nnf_r_6 = (Dsj [nnf_r_2, nnf_r_1, nnf_r_2])

nnf_f_7 = (Neg nnf_f_5)
nnf_r_7 = (Cnj [nnf_r_2, nnf_r_1, nnf_r_2])

nnf_check_1 = (nnf nnf_f_1) == nnf_r_1
nnf_check_2 = (nnf nnf_f_2) == nnf_r_2
nnf_check_3 = (nnf nnf_f_3) == nnf_r_3
nnf_check_4 = (nnf nnf_f_4) == nnf_r_4
nnf_check_5 = (nnf nnf_f_5) == nnf_r_5
nnf_check_6 = (nnf nnf_f_6) == nnf_r_6
nnf_check_7 = (nnf nnf_f_7) == nnf_r_7

-- CNF tests.
cnf_f_1 = p
cnf_r_1 = p

cnf_f_2 = (Neg q)
cnf_r_2 = (Neg q)

cnf_f_3 = (Cnj [nnf_f_1, nnf_f_2])
cnf_r_3 = (Cnj [nnf_r_1, nnf_r_2])

cnf_f_4 = (Dsj [nnf_f_1, nnf_f_2])
cnf_r_4 = (Dsj [nnf_r_1, nnf_r_2])

cnf_f_5 = (Dsj [(Cnj [p, q]), r])
cnf_r_5 = (Cnj [(Dsj [p, r]), (Dsj [q, r])])

cnf_f_6 = (Dsj [p, (Cnj [q, r])])
cnf_r_6 = (Cnj [(Dsj [p, q]), (Dsj [p, r])])

cnf_f_7 = (Dsj [(Cnj [p, q, r]), (Prop 4)])
cnf_r_7 = (Cnj [(Dsj [p, (Prop 4)]), (Dsj [q, (Prop 4)]), (Dsj [r, (Prop 4)])])

cnf_f_8 = (Dsj [p, q, (Cnj [r, (Prop 4)])])
cnf_r_8 = (Cnj [(Dsj [(Dsj [p, q]), r]), (Dsj [(Dsj [p, q]), (Prop 4)])])

cnf_f_9 = (Dsj [p, (Cnj [q, r, (Prop 4)])])
cnf_r_9 = (Cnj [(Dsj [p, q]), (Dsj [p, r]), (Dsj [p, (Prop 4)])])

cnf_check_1 = (cnf cnf_f_1) == cnf_r_1
cnf_check_2 = (cnf cnf_f_2) == cnf_r_2
cnf_check_3 = (cnf cnf_f_3) == cnf_r_3
cnf_check_4 = (cnf cnf_f_4) == cnf_r_4
cnf_check_5 = (cnf cnf_f_5) == cnf_r_5
cnf_check_6 = (cnf cnf_f_6) == cnf_r_6
cnf_check_7 = (cnf cnf_f_7) == cnf_r_7
cnf_check_8 = (cnf cnf_f_8) == cnf_r_8
cnf_check_9 = (cnf cnf_f_9) == cnf_r_9

-- to CNF alternative
toCNF' :: Form -> Form
toCNF' = cnf . nnf . arrowfree'

-- precondition: any formula.
arrowfree' :: Form -> Form 
arrowfree' (Prop x) = Prop x 
arrowfree' (Neg f) = Neg (arrowfree' f)
arrowfree' (Cnj fs) = Cnj (map arrowfree' fs)
arrowfree' (Dsj fs) = Dsj (map arrowfree' fs)
arrowfree' (Impl f1 f2) = Dsj [Neg (arrowfree' f1), arrowfree' f2]
arrowfree' (Equiv f1 f2) = Cnj [(Dsj [Neg f1', f2']), (Dsj [f1', Neg f2'])]
  where f1' = arrowfree' f1
        f2' = arrowfree' f2
