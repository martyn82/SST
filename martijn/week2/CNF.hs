module CNF
where

import Week2
import Logic

-- translate formula to conjunctive normal form (CNF)
-- precondition : input is arrow-free and in NNF
-- postcondition: output is in CNF
cnf :: Form -> Form
cnf (Prop x)      = Prop x
cnf (Neg(Prop x)) = Neg (Prop x) -- this one could be simplified, but this way we catch improper inputs
cnf (Cnj fs)      = Cnj (map cnf fs)
cnf (Dsj (f:fs))  = foldl dist (cnf f) (map cnf fs)

-- apply distributive law to two formulas in CNF
-- precondition : inputs are arrow-free formulas in NNF
-- postcondition: output is CNF of x OR y
dist :: Form -> Form -> Form
dist (Cnj xs) ys = (Cnj [dist x ys | x <- xs])
dist xs (Cnj ys) = (Cnj [dist xs y | y <- ys])
dist xs ys       = (Dsj [xs, ys])

-- composed function to translate a formula into CNF
-- precondition : input is any formula
-- postcondition: output is an equivalent formula in CNF
translateCNF :: Form -> Form
translateCNF = cnf . nnf . arrowfree

-- TEST PLAN:
-- The translateCNF function relays on three other functions: cnf, nnf, and arrowfree; and applies them without mutations.
-- Each of these functions have rules implemented to translate an input formula to an output formula.
-- This is a mapping of inputs to outputs. The language has infinite possible formulas, but there are a finite number of rules.
-- The test cases here tests each of the functions by checking their pre- and postconditions.
-- If we test each rule and none of the tests fail, we can conclude that the functions
-- are correct and inductively conclude that the composition of the functions is also correct.

-- test cases
-- arrow free
arrowfreetests = [(p           , p),
                  ((Neg p)     , (Neg p)),
                  ((Cnj [p, q]), (Cnj [p, q])),
                  ((Dsj [p, q]), (Dsj [p, q])),
                  ((Impl p q)  , (Dsj [Neg p, q])),
                  ((Equiv p q) , (Dsj [(Cnj [p, q]), (Cnj [Neg p, Neg q])]))]
-- negation normal form
nnftests       = [(p            , p),
                  ((Neg p)      , (Neg p)),
                  ((Neg (Neg p)), p),
                  ((Cnj [p, q]) , (Cnj [p, q])),
                  ((Dsj [p, q]) , (Dsj [p, q]))]
-- conjunctive normal form
cnftests       = [(p           , p),
                  ((Neg p)     , (Neg p)),
                  ((Cnj [p, q]), (Cnj [p, q])),
                  ((Dsj [p, q]), (Dsj [p, q]))]
-- test cases of distribution of formulas
disttests      = [((Cnj [p, q]), (r), (Cnj [(Dsj [p, r]), (Dsj [q, r])])),
                  ((p), (Cnj [q, r]), (Cnj [(Dsj [p, q]), (Dsj [p, r])]))]

-- combined CNF test cases
allCNFTests = [(arrowfreetests, arrowfree, "ArrowFree"),
               (nnftests, nnf, "NNF"),
               (cnftests, cnf, "CNF")]

-- combined DIST test cases
allDistTests   = [(disttests, dist, "Distribution")]

-- test translation functions
testCNF :: [(Form, Form)] -> (Form -> Form) -> Bool
testCNF tests fn = and (map (\ (item, expected) -> (fn item) == expected && (equiv (fn item) expected)) tests)

-- test distribution function
testDist :: [(Form, Form, Form)] -> (Form -> Form -> Form) -> Bool
testDist tests fn = and (map (\ (item1, item2, expected) -> (fn item1 item2) == expected && (equiv (fn item1 item2) expected)) tests)

-- run CNF tests
runCNFTests :: [([Char], Bool)]
runCNFTests = map (\ (testcases, fn, name) -> (name, (testCNF testcases fn))) allCNFTests

-- run dist tests
runDistTests :: [([Char], Bool)]
runDistTests = map (\ (testcases, fn, name) -> (name, (testDist testcases fn))) allDistTests

-- run test suite
runAllTests :: [([Char], Bool)]
runAllTests = runCNFTests ++ runDistTests

