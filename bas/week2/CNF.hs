module CNF

where 

import Week2
import PropositionTests


createInvertedVariable :: (Name, Bool) -> Form
createInvertedVariable (name, value) | value 	  = Neg (Prop name)
									 | otherwise = Prop name
									 
allFalses func = (filter (\val -> not (eval val func)) (allVals func))
										
cnf :: Form -> Form	
cnf (Prop prop) = (Prop prop)
cnf (func) | isCnf func       = func
		   | tautology func   = Dsj [p, Neg (p)]
		   | length dsjs == 1 = head dsjs
		   | otherwise 	      = Cnj dsjs
		   where dsjs = (map (\val -> Dsj (map (\var -> createInvertedVariable var) val)) (allFalses func))

isDsj (Dsj func)  = and $ map isDsj func
isDsj (Neg func)  = isDsj func
isDsj (Prop prop) = True
isDsj func        = False
		
isCnf (Cnj func)  = and $ map isCnf func
isCnf func        = isDsj func