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
cnf (func) | isCnf func     = func
		   | tautology func = Dsj [p, Neg (p)]
		   | otherwise 	    = Cnj (map (\val -> Dsj (map (\var -> createInvertedVariable var) val)) (allFalses func))

	   
	   
	   
		   
isProp (Prop prop) = True
isProp func        = False

isNeg (Neg func) = True
isNeg func       = False

isDsj (Dsj func) = and $ map isDsj func
isDsj func       | isProp func = True
                 | isNeg func = True
		         | otherwise = False
		
isCnf (Cnj func)  = and $ map isCnf func
isCnf func        = isDsj func