module Uncurry
where

-- expand 3-tuple to separate function arguments
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 fn = \(a,b,c) -> fn a b c

