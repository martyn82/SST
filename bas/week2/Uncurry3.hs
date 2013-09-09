module Uncurry3

where
--helper to run the tests
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 fn =  \(a,b,c) -> fn a b c