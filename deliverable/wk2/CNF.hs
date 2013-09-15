module CNF
where

import Week2
import Logic

-- we have two possible solutions
-- the first follows the assignment: 

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
toCnf :: Form -> Form
toCnf = cnf . nnf . arrowfree


--the second solution has a 'black box' approach
--on the function, find all values that evaluate
--to false, create a conjunction from the disjunctions
--for the inverted negative values. Each disjunction 
--causes another row in the truth table to evaluate to false.

createInvertedVariable :: (Name, Bool) -> Form
createInvertedVariable (name, value) | value 	 = Neg (Prop name)
									 | otherwise = Prop name
									 
allFalses func = (filter (\val -> not (eval val func)) (allVals func))
							
-- precondition : any formula
-- postcondition: output is in CNF, equivalent to input	
toCnf' :: Form -> Form	
toCnf' (Prop prop) = (Prop prop)
toCnf' (func) | isCnf func       = func				 -- don't convert if already cnf
			  | tautology func   = Dsj [p, Neg (p)]   -- a tautology will yield an empty set of false evaluatons, so we create a simple tautology based on the first variable
			  | length dsjs == 1 = head dsjs          -- if a single disjunction exists, it does not require a conjunction
			  | otherwise 	      = Cnj dsjs            
			  where dsjs = (map (\val -> Dsj (map (\var -> createInvertedVariable var) val)) (allFalses func))
					 
					 
-- TEST PLAN:
-- The toCnf function relays on three other functions: cnf, nnf, and arrowfree; and applies them without mutations.
-- Each of these functions have rules implemented to translate an input formula to an output formula.
-- This is a mapping of inputs to outputs. The language has infinite possible formulas, but there are a finite number of rules.
-- The test cases here tests each of the functions by checking their pre- and postconditions.
-- If we test each rule and none of the tests fail, we can conclude that the functions
-- are correct and conclude that the composition of the functions is also correct.

-- test cases
-- arrow free
arrowfreetests = [(p           , p),
                  ((Neg p)     , (Neg p)),
                  ((Cnj [p, q]), (Cnj [p, q])),
                  ((Dsj [p, q]), (Dsj [p, q])),
                  ((Impl p q)  , (Dsj [Neg p, q])),
                  ((Equiv p q) , (Dsj [(Cnj [p, q]), (Cnj [Neg p, Neg q])]))]
				  
				  
-- check for no errors
isArrowFree (Prop name) = True
isArrowFree (Neg f)  = isArrowFree f
isArrowFree (Cnj fs) = and (map isArrowFree fs)
isArrowFree (Dsj fs) = and (map isArrowFree fs)
isArrowFree (Impl f1 f2) = False
isArrowFree (Equiv f1 f2) = False				  
				  
-- negation normal form
nnftests       = [(p            , p),
                  ((Neg p)      , (Neg p)),
                  ((Neg (Neg p)), p),
                  ((Cnj [p, q]) , (Cnj [p, q])),
                  ((Dsj [p, q]) , (Dsj [p, q]))]

--check for NNF
isNnf fn = (isArrowFree fn) && (isNnf' fn)				  
isNnf' :: Form -> Bool
isNnf' (Prop x) = True
isNnf' (Neg (Prop x)) = True
isNnf' (Neg (Neg f)) = False
isNnf' (Cnj fs) = and (map isNnf fs)
isNnf' (Dsj fs) = and (map isNnf fs)
isNnf' (Neg (Cnj fs)) = False
isNnf' (Neg (Dsj fs)) = False

-- conjunctive normal form
cnftests       = [(p           , p),
                  ((Neg p)     , (Neg p)),
                  ((Cnj [p, q]), (Cnj [p, q])),
                  ((Dsj [p, q]), (Dsj [p, q]))]

				  
--function to test CNF structure.
--following the grammar of CNF:
-- L ::= p | !p
-- D ::= L | L v D
-- C ::= D | D ^ C
		   
isLiteral (Prop func)     = True
isLiteral (Neg (Prop f))  = True	
isLiteral func		      = False	   
	
isDsj (Dsj func)  = and $ map isDsj func
isDsj func        = isLiteral func
		
isCnf (Cnj func)  = and $ map isCnf func
isCnf func        = isDsj func				  
				  
-- test cases of distribution of formulas
disttests      = [((Cnj [p, q]), (r), (Cnj [(Dsj [p, r]), (Dsj [q, r])])),
                  ((p), (Cnj [q, r]), (Cnj [(Dsj [p, q]), (Dsj [p, r])]))]

-- combined CNF test cases
allCNFTests = [(arrowfreetests, arrowfree, "ArrowFree"),
               (nnftests, nnf, "NNF"),
               (cnftests, cnf, "CNF")]

-- combined DIST test cases
allDistTests   = [(disttests, dist, "Distribution")]

-- test translation functions by checking if input and output match expectations, and if input and output are equivalent
testCNF :: [(Form, Form)] -> (Form -> Form) -> Bool
testCNF tests fn = (and (map (\ (item, expected) -> (fn item) == expected && (equiv (fn item) expected)) tests))

-- test distribution function by checking if input and output match expectations, and if input and output are equivalent
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





