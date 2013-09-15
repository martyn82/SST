module PropositionTests

where 

import Week2
import Uncurry3

-- 2. Implement and test programs for (propositional) contradiction,
-- tautology, logical entailment and logical equivalence.

--helper function, returns all end outcomes of a truth table for a form
allResults form = map (\val -> eval val form) (allVals form)

--all values must be false regardless of input
contradiction a = not (or (allResults a))
--all values must be true regardless of input
tautology a = and (allResults a)

-- b is a logical consequence of a if a ==> b is logically valid
entailment a b = (allResults a) <= (allResults b)

--a is logically equivalent to b if a <=> b is logically valid
equivalent a b = (allResults a) == (allResults b)

check1 = Cnj [(Neg p), p]
check2 = Dsj [(Neg p), p]
check3 = Dsj [p, q]
check4 = Cnj [p, q]
check5 = Cnj [(Cnj [p, q, r]), (Neg r)]
check6 = Dsj [check1, check2]


contradictions = [check1, check5]
tautologies    = [check2, check6]
satisfiables   = [check3, check4]

runTests :: [Form] -> (Form -> Bool) -> Bool -> Bool
runTests forms func expected = and (map (\form -> (func form) == expected) forms)

allTests1 = [(contradictions, tautology, False),
             (satisfiables, tautology, False),
			 (tautologies, tautology, True),
			 (contradictions, contradiction, True),
			 (satisfiables, contradiction, False),
			 (tautologies, contradiction, False)]
 
runAllTests1 = and (map (uncurry3 runTests) allTests1)