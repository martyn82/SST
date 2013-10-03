module Week5

where

import Data.List
import Week4 

pre1 :: (a -> Bool) -> String -> (a -> b) -> a -> b 
pre1 p e f x = if p x then f x 
             else error e

post1 :: (b -> Bool) -> String -> (a -> b) -> a -> b 
post1 p e f x = if p (f x) then f x 
              else error e

decomp :: Integer -> (Integer,Integer)
decomp n = decomp' (0,n) where
  decomp' = while1 (\ (_,m) -> even m)
                   (\ (k,m) -> (k+1,m `div` 2))

decompPost = post1 (\ (_,m) -> odd m) "m in result should be odd"  decomp

assert1 :: (a -> b -> Bool) -> String -> (a -> b) -> a -> b 
assert1 p e f x = if p x (f x) then f x 
                else error e

decompA = assert1 (\ n (k,m) -> n == 2^k*m) "for resulting (k,m) -> n should be 2^k*m"  decomp

invar1 :: (a -> Bool) -> (a -> a) -> a -> a
invar1 p f x = 
  let 
    x' = f x 
  in
  if p x && not (p x') then error "invar1"
  else x'

gSign = invar1 (>0) (while1 even (`div` 2))

gSign' = invar1 (<0) (while1 even (`div` 2))

decompInvar :: Integer -> (Integer,Integer)
decompInvar n = decomp' (0,n) where
  decomp' = while1 (\ (_,m) -> even m)
                   (invar1 
                     (\ (i,j) -> 2^i*j == n)
                     (\ (k,m) -> (k+1,m `div` 2)))

infix 1 ==> 

(==>) :: Bool -> Bool -> Bool
p ==> q = (not p) || q

sortedProp :: Ord a => [a] -> [a] -> [a] -> Bool
sortedProp xs ys zs = 
  (sorted xs && sorted ys) ==> sorted zs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True 
sorted (x:y:zs) = x <= y && sorted (y:zs)

sublistProp :: Eq a => [a] -> [a] -> [a] -> Bool
sublistProp xs ys zs = 
  sublist xs zs && sublist ys zs

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x:xs) ys = 
  elem x ys && sublist xs (ys \\ [x])

assert2 ::  (a -> b -> c -> Bool) 
             -> (a -> b -> c) -> a -> b -> c
assert2 p f x y = 
  if p x y (f x y) then f x y
  else error "assert2"

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y 
                         then x : merge xs (y:ys) 
                         else y : merge (x:xs) ys

mergeA :: Ord a => [a] -> [a] -> [a]
mergeA = assert2 sortedProp 
            $ assert2 sublistProp merge

ext_gcd :: Integer -> Integer -> (Integer,Integer) 
ext_gcd a b = let 
   x = 0
   y = 1
   lastx = 1
   lasty = 0
  in ext_gcd' a b x y (lastx,lasty) 

ext_gcd' = while5 (\ _ b _ _ _ ->  b /= 0) 
                  (\ a b x y (lastx,lasty) -> let 
                    (q,r)   = quotRem a b 
                    (x',lastx') = (lastx-q*x,x)
                    (y',lasty') = (lasty-q*y,y)
                 in (b,r,x',y',(lastx',lasty')))

while5 :: (a -> b -> c -> d -> e -> Bool)
       -> (a -> b -> c -> d -> e -> (a,b,c,d,e))
       -> a -> b -> c -> d -> e -> e   
while5 p f x y z v w
  | p x y z v w = let 
                    (x',y',z',v',w') = f x y z v w
                  in while5 p f x' y' z' v' w'
  | otherwise = w

bezout :: Integer -> Integer 
          -> (Integer,Integer) -> Bool
bezout m n (x,y) = x*m + y*n == gcd m n 

ext_gcdA = assert2 bezout ext_gcd

fct_gcd :: Integer -> Integer -> (Integer,Integer) 
fct_gcd a b = 
  if b == 0 
  then (1,0) 
  else 
     let 
       (q,r) = quotRem a b
       (s,t) = fct_gcd b r 
     in (t, s - q*t)

divides :: Integer -> Integer -> Bool
divides n m = rem m n == 0

gcd_property :: Integer -> Integer -> (Integer,Integer) -> Bool
gcd_property = \ m n (x,y) -> let 
    d = x*m + y*n 
  in 
    divides d m && divides d n 

fct_gcdA = assert2 gcd_property fct_gcd

assert3 :: (a -> b -> c -> d -> Bool) -> 
           (a -> b -> c -> d) -> 
            a -> b -> c -> d
assert3 p f x y z = 
  if p x y z (f x y z) then f x y z
  else error "assert3"

invar3 :: (a -> b -> c -> Bool) -> 
          (a -> b -> c -> (a,b,c)) -> 
           a -> b -> c -> (a,b,c)
invar3 p f x y z = 
  let 
    (x',y',z') = f x y z
  in 
   if p x y z && not (p x' y' z') then error "invar3"
   else (x',y',z')

assert4 ::  (a -> b -> c -> d -> e -> Bool) 
             -> (a -> b -> c -> d -> e) 
             -> a -> b -> c -> d -> e
assert4 p f x y z u = 
  if p x y z u (f x y z u) then f x y z u
  else error "assert4"

invar4 :: (a -> b -> c -> d -> Bool) -> 
          (a -> b -> c -> d -> (a,b,c,d)) -> 
           a -> b -> c -> d -> (a,b,c,d)
invar4 p f x y z u = 
  let 
    (x',y',z',u') = f x y z u
  in 
   if p x y z u && not (p x' y' z' u')
   then error "invar4"
   else (x',y',z',u')

assert5 ::  (a -> b -> c -> d -> e -> f -> Bool) 
             -> (a -> b -> c -> d -> e -> f) 
             -> a -> b -> c -> d -> e -> f
assert5 p f x y z u v = 
  if p x y z u v (f x y z u v) then f x y z u v
  else error "assert5"

invar5 :: (a -> b -> c -> d -> e -> Bool) -> 
          (a -> b -> c -> d -> e -> (a,b,c,d,e)) -> 
           a -> b -> c -> d -> e -> (a,b,c,d,e)
invar5 p f x y z u v = 
  let 
    (x',y',z',u',v') = f x y z u v
  in 
   if p x y z u v && not (p x' y' z' u' v')
   then error "invar5"
   else (x',y',z',u',v')
