module TestCNF where

import Techniques

import Week2
import Logic
import CNF

-- Exercise 6 : Time spent 3 hours
-- Formula traverse method.
traverse :: (Form -> Bool) -> Form -> Bool
traverse _ (Prop f) = True
traverse x (Neg f) = x f
traverse x (Impl f g) = x f && x g
traverse x (Equiv f g) = x f && x g
traverse x (Cnj fs) = all x fs
traverse x (Dsj fs) = all x fs

-- CNF properties:
-- - No arrows
testCNFNoArrows :: Form -> Bool
testCNFNoArrows (Impl f g) = False
testCNFNoArrows (Equiv f g) = False
testCNFNoArrows x = traverse testCNFNoArrows x

-- - Negation is only possible on Atoms
testCNFNegationOnAtoms :: Form -> Bool
testCNFNegationOnAtoms (Neg (Prop f)) = True
testCNFNegationOnAtoms (Neg f) = False
testCNFNegationOnAtoms x = traverse testCNFNegationOnAtoms x

-- - There cannot be Conjunctions in Disjunctions
testCNFNoCnjInDsj :: Form -> Bool
testCNFNoCnjInDsj (Dsj fs) = all testCNFNoCnjInDsj' fs
testCNFNoCnjInDsj x = traverse testCNFNoCnjInDsj x

-- precondition: Form is in a Disjunction
testCNFNoCnjInDsj' :: Form -> Bool
testCNFNoCnjInDsj' (Cnj fs) = False
testCNFNoCnjInDsj'  x = traverse testCNFNoCnjInDsj' x

-- Test all properties
-- Test n forms (that are converted to CNF by toCnf) for CNF properties
testFormForCNFProperties :: Int -> IO ()
testFormForCNFProperties n = testFormForProperties n [testCNFNoArrows, testCNFNegationOnAtoms, testCNFNoCnjInDsj]

testFormForProperties :: Int -> [(Form -> Bool)] -> IO ()
testFormForProperties n props = do fs <- getRandomFs n
                                   testForProperties n props fs

testForProperties :: Int -> [(Form -> Bool)] -> [Form] -> IO ()
testForProperties n _ [] = print (show n ++ " tests passed")
testForProperties n ps (f:fs) = if isCNF
                                then do print ("pass on:" ++ show f)
                                        testForProperties n ps fs
                                else error ("failed test on:" ++ show f)
                                where cnf = toCnf f
                                      isCNF = and [p cnf | p <- ps]

-- Test results:
-- Random formula generator generates formulas like (Dsj []) and (Cnj []). That is a bit weird, but my program crashes on formulas of the kind of (Dsj []). I've added the case "cnf (Dsj []) = Dsj []"
-- I tend to say that the random formula generator is broken, but the precondition for toCNF is 'any formula'. So I do guess this also includes invalid formulas.
--
-- Later I found out that the random formula generator does not generate arrows.. Too bad I included some tests for that.
--
-- "pass on:3"
-- "pass on:12"
-- "pass on:*(*(19 8 8 8 16) *(10 19 2 12))"
-- "pass on:*(1)"
-- "pass on:*(4 *(12) 5)"
-- "pass on:*(*(16 12 12 15 18) *(2 6 20 9) *(19) 11 7)"
-- "10000 tests passed"
