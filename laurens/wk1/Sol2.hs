module Sol2 where

import TAMO

-- 2.9
isTwoDotNineValid = valid2 (\p q -> ((p <+> q) <+> q) <=> p)

-- 2.13
test2_12_1a = lequiv (not True) False
test2_12_1b = lequiv (not False) True
test2_12_2  = lequiv (\p -> p ==> False) (\p -> not p)
test2_12_3a = lequiv (\p -> p || True) (\_ -> True)
test2_12_3b = lequiv (\p -> p && False) (\_ -> False)
test2_12_4a = lequiv (\p -> p || False) (\p -> p)
test2_12_4b = lequiv (\p -> p && True) (\p -> p)
test2_12_5  = lequiv (\p -> p || not p) (\_ -> True)
test2_12_6  = lequiv (\p -> p && not p) (\_ -> False)

-- 2.15
contra1 :: (Bool -> Bool) -> Bool
contra1 f = (f True <=> False) && (f False <=> False)

contra2 :: (Bool -> Bool -> Bool) -> Bool
contra2 f = and [f p q <=> False | p <- [True, False]
                                 , q <- [True, False]]

contra3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contra3 f = and [f p q r <=> False | p <- [True, False]
			           , q <- [True, False]
                                   , r <- [True, False]]

-- 2.20
test_2_20_1 = lequiv (\p q -> not p ==> q) (\p q -> p ==> not q)
test_2_20_2 = lequiv (\p q -> not p ==> q) (\p q -> q ==> not p)
test_2_20_3 = lequiv (\p q -> not p ==> q) (\p q -> not q ==> p)
test_2_20_4 = lequiv (\p q r -> p ==> (q ==> r)) (\p q r -> q ==> (p ==> r))
test_2_20_5 = lequiv (\p q r -> p ==> (q ==> r)) (\p q r -> (p ==> q) ==> r)
test_2_20_6 = lequiv (\p q -> (p ==> q) ==> p) (\p _ -> p)
test_2_20_7 = lequiv (\p q r -> p || q ==> r) (\p q r -> (p ==> r) && (q ==> r))
