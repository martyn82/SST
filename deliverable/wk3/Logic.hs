module Logic
where

import Week2

-- THIS IS A COPY OF THE FILE OF WEEK2

-- A contradiction is a formula that is not satisfiable
contradiction :: Form -> Bool
-- VVZ: correct, but a shorter notation would be "contradiction = not . satisfiable"
contradiction f = not (satisfiable f)

-- A tautology is a formula for which all valuations are True
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- Logical entailment is the property of two formulas if for all valuations V(a) = True holds that V(b) = True
entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

-- Logical equivalence is the property of two formulas that have the same truth table (therefore, have the exact same valuations)
equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b)



-- TEST PLAN
-- Below the test cases are defined for each function.

-- always false
contradictions = [(Cnj [p, (Neg p)]),                           -- p AND ~p
                  (Equiv p (Neg p)),                            -- p <-> ~p
                  (Neg (Dsj [p, (Neg p)])),                     -- ~(p OR ~p)
                  (Impl (Dsj [p, (Neg p)]) (Cnj [p, (Neg p)]))] -- (p OR ~p) -> (p AND ~p)
-- always true
tautologies    = [(Dsj [p, (Neg p)]),                           -- p OR ~p
                  (Equiv p p),                                  -- p <-> p
                  (Impl (Cnj [p, q]) q)]                        -- (p AND q) -> q
-- entailment
entailments    = [(p, p),                                       -- p |= p
                 ((Cnj [p, q]), (Dsj [p, q])),                  -- p AND q |= p OR q
                 ((Cnj [p, q]), p)]                             -- p AND q |= p
-- same valuations
equivalences   = [((Cnj [p, q]), (Neg(Dsj [Neg(p), Neg(q)])))]  -- (p AND q) <=> ~(~p OR ~q)

-- combined test cases
allUnaryTests = [(contradictions, contradiction, "Contradictions"),
                  (tautologies, tautology, "Tautologies")]
allBinaryTests = [(entailments, entails, "Entailments"),
                   (equivalences, equiv, "Equivalences")]

-- single assertion
testUnary :: [Form] -> (Form -> Bool) -> Bool
testUnary tests fn = and (map (\ item -> fn item) tests)

-- compare assertion
testBinary :: [(Form, Form)] -> (Form -> Form -> Bool) -> Bool
testBinary tests fn = and (map (\ (item1, item2) -> fn item1 item2) tests)

-- run test suite for unary functions
runUnaryTests :: [([Char],Bool)]
runUnaryTests = map (\ (testCase, fn, name) -> (name, (testUnary testCase fn))) allUnaryTests

-- run test suite for comparing formulas
runBinaryTests :: [([Char],Bool)]
runBinaryTests = map (\ (testCase, fn, name) -> (name, (testBinary testCase fn))) allBinaryTests

-- run whole test suite
runLogicTests :: [([Char],Bool)]
runLogicTests = runUnaryTests ++ runBinaryTests
