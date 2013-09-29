module Rel
where

import Data.List

import Set

-- Relations
-- binary relation type
type Rel a = [(a,a)]

-- 'on' operator for relations
infixr 5 @@

-- 'on' is defined for operands of type relation Rel and yields a new relation Rel
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

-- inserts an element in a list
insertList :: Ord a => a -> [a] -> [a]
insertList x []         = [x]
insertList x ys@(y:ys') = case compare x y of
                               GT -> y : insertList x ys'
                               EQ -> ys
                               _  -> x : ys

-- inserts an element in a relation
insertRel :: (Ord a) => a -> [a] -> [a]
insertRel x r = insertList x r

-- computes the union for relations
unionRel :: Ord a => Rel a -> Rel a -> Rel a
unionRel [] y     = y
unionRel (x:xs) y = insertRel x (unionRel xs y)

-- compute the transitive closure R+ of a relation R
trClos :: Ord a => Rel a -> Rel a
trClos r = unionRel r (r @@ r)

