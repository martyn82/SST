    module TestCNF
where

import Week2;

-- Exercise 6 : Time spent 3 hours
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

