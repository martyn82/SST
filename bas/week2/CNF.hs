module CNF

where 

import Week2
import PropositionTests


createInvertedVariable :: (Name, Bool) -> Form
createInvertedVariable (name, value) | value 	  = Neg (Prop name)
									 | otherwise = Prop name
									  

allFalses func = (filter (\val -> not (eval val func)) (allVals func))
									  
cnf :: Form -> Form	
cnf (func) | tautology func  = Dsj ((map (\name -> Prop name) (propNames func)) ++ [Neg (p)])
		   | otherwise 		 =  Cnj (map (\val -> Dsj (map (\var -> createInvertedVariable var) val)) (allFalses func))
		   
