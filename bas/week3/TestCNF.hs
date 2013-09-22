module TestCNF
where

import Week2;

-- Exercise 6 : Time spent 3 hours
booleanTraversal :: Form -> Bool
booleanTraversal (Prop f) = True
booleanTraversal (Neg f) = booleanTraversal f
booleanTraversal (Impl f g) = booleanTraversal f && booleanTraversal g
booleanTraversal (Equiv f g) = booleanTraversal f && booleanTraversal g
booleanTraversal (Cnj fs) = all booleanTraversal fs
booleanTraversal (Dsj fs) = all booleanTraversal fs

-- CNF properties:
-- - No arrows
testCNFNoArrows :: Form -> Bool
testCNFNoArrows (Impl f g) = False
testCNFNoArrows (Equiv f g) = False
testCNFNoArrows x = booleanTraversal x

-- - Negation is only possible on Atoms
testCNFNegationOnAtoms :: Form -> Bool
testCNFNegationOnAtoms (Neg (Prop f)) = True
testCNFNegationOnAtoms (Neg f) = False
testCNFNegationOnAtoms x = booleanTraversal x

-- - There cannot be Conjunctions in Disjunctions
testCNFNoCnjInDsj :: Form -> Bool
testCNFNoCnjInDsj (Dsj fs) = all testCNFNoCnjInDsj' fs
testCNFNoCnjInDsj x = booleanTraversal x

-- precondition: Form is in a Disjunction
testCNFNoCnjInDsj' :: Form -> Bool
testCNFNoCnjInDsj' (Cnj fs) = False
testCNFNoCnjInDsj'  x = booleanTraversal x

