module CNFTest where

import Techniques
import Week2
import Lab2

-- Exercise 6 : Time spent 3 hours
-- CNF properties:
-- - No arrows
testCNFNoArrows :: Form -> Bool
testCNFNoArrows (Prop f) = True
testCNFNoArrows (Impl f g) = False
testCNFNoArrows (Equiv f g) = False
testCNFNoArrows (Neg f) = testCNFNoArrows f
testCNFNoArrows (Cnj fs) = all testCNFNoArrows fs
testCNFNoArrows (Dsj fs) = all testCNFNoArrows fs

-- - Negation is only possible on Atoms
testCNFNegationOnAtoms :: Form -> Bool
testCNFNegationOnAtoms (Prop f) = True
testCNFNegationOnAtoms (Neg (Prop f)) = True
testCNFNegationOnAtoms (Neg f) = False
testCNFNegationOnAtoms (Cnj fs) = all testCNFNegationOnAtoms fs
testCNFNegationOnAtoms (Dsj fs) = all testCNFNegationOnAtoms fs
testCNFNegationOnAtoms (Impl f g) = all testCNFNegationOnAtoms [f, g]
testCNFNegationOnAtoms (Equiv f g) = all testCNFNegationOnAtoms [f, g]

-- - There cannot be Conjunctions in Disjunctions
testCNFNoCnjInDsj :: Form -> Bool
testCNFNoCnjInDsj (Prop f) = True
testCNFNoCnjInDsj (Neg f) = testCNFNoCnjInDsj f
testCNFNoCnjInDsj (Dsj fs) = all testCNFNoCnjInDsj' fs
testCNFNoCnjInDsj (Cnj fs) = all testCNFNoCnjInDsj fs
testCNFNoCnjInDsj (Impl f g) = all testCNFNoCnjInDsj [f, g]
testCNFNoCnjInDsj (Equiv f g) = all testCNFNoCnjInDsj [f, g]

-- precondition: Form is in a Disjunction
testCNFNoCnjInDsj' :: Form -> Bool
testCNFNoCnjInDsj' (Prop f) = True
testCNFNoCnjInDsj' (Cnj fs) = False
testCNFNoCnjInDsj' (Dsj fs) = all testCNFNoCnjInDsj' fs
testCNFNoCnjInDsj' (Neg f) = testCNFNoCnjInDsj' f
testCNFNoCnjInDsj' (Impl f g) = all testCNFNoCnjInDsj' [f, g]
testCNFNoCnjInDsj' (Equiv f g) = all testCNFNoCnjInDsj' [f, g]

-- Test all properties
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
                                where cnf = toCNF f
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
