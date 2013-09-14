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
-- precondition : inputs are in CNF
-- postcondition: output is CNF of (x OR y)
dist :: Form -> Form -> Form
dist (Cnj xs) ys = (Cnj [dist x ys | x <- xs])
dist xs (Cnj ys) = (Cnj [dist xs y | y <- ys])
dist xs ys       = (Dsj [xs, ys])

-- composed function to translate a formula into CNF
-- precondition : input is any formula
-- postcondition: output is an equivalent formula in CNF
translateCNF :: Form -> Form
translateCNF = cnf . nnf . arrowfree


