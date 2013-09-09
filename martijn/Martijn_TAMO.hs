module Martijn_TAMO
where

import TAMO

-- Ex.2.2 Truth table for Exclusive Or
-- -------------------
-- | P | Q | P xor Q |
-- +-----------------+
-- | t | t |    f    |
-- | t | f |    t    |
-- | f | t |    t    |
-- | f | f |    f    |
-- -------------------

-- Ex.2.4 Truth table for Negated Equivalence:
-- ----------------------
-- | P | Q | !(P <=> Q) |
-- +--------------------+
-- | t | t | f          |
-- | t | f | t          |
-- | f | t | t          |
-- | f | f | f          |
-- ----------------------
-- This is indeed the exact same truth table as Exclusive Or; it has the same
-- truth values for the formula with the same truth values for the propositions.
-- The Haskell implementation of Exclusive Or (<+>) which is defined as:
--     x <+> y = x /= y
-- only yields TRUE whenever the propositions are inequal and will again produce
-- the same truth table.

-- Ex.2.9 Truth table showing equivalence of ((P xor Q) xor Q) and P
-- ---------------------------
-- | P | Q | (P xor Q) xor Q |
-- +-------------------------+
-- | t | t |            t    |
-- | t | f |            t    |
-- | f | t |            f    |
-- | f | f |            f    |
-- ---------------------------

-- Ex.2.11: -

-- Ex.2.13 Checks for principles of Theorem 2.12
theorem1a = (not True)
theorem1b = (not False)
theorem2  = lequiv (\ p -> (p ==> False)) (\ p -> (not p))
theorem3a = lequiv (\ p -> (p || True)) (\ p -> (True))
theorem3b = lequiv (\ p -> (p && False)) (\ p -> (False))
theorem4a = lequiv (\ p -> (p || False)) (\ p -> (p))
theorem4b = lequiv (\ p -> (p && True)) (\ p -> (p))
theorem5  = lequiv (\ p -> (p || (not p))) (\ p -> (True))
theorem6  = lequiv (\ p -> (p && (not p))) (\ p -> (False))

-- Ex.2.15 Contradictions
contra1a = lequiv (\ p -> False) (\ p -> (p && (not p)))
contra1b = lequiv (\ p -> False) (\ p -> (p <=> (not p)))
contra2a = lequiv (\ p q -> False) (\ p q -> ((p && (not p)) && (q && (not q))))
contra3a = lequiv (\ p q r -> False) (\ p q r -> (((p <=> (not p)) && (q <=> (not q))) && (r && (not r))))

-- Ex.2.16 Denials for 2.13
-- Don't know what is expected. We could easily wrap each formula in a NOT operator, but would that be 'useful'?

-- Ex.2.17 (?)

-- Ex.2.18 
ex218a = lequiv (\ p q -> (p <=> q)) (\ p q -> ((not p) <=> (not q)))
ex218b = lequiv (\ p q -> ((not p) <=> q)) (\ p q -> (p <=> (not q)))

-- Ex.2.19
ex219 = (valid (\ p q -> p <=> q)) <=> (lequiv p q)

-- Ex.2.20 Are the following pairs of formulas equivalent?
-- 1. No
-- P | Q | !P => Q | P => !Q
-- t | t |     t   |   f

-- 2. No
-- P | Q | !P => Q | Q => !P
-- t | t |    t    |    f

-- 3. Yes
-- P | Q | !P => Q | !Q => P
-- t | t |    t    |    t
-- t | f |    t    |    t
-- f | t |    t    |    t
-- f | f |    f    |    f

-- 4. Yes
-- P | Q | R | P => (Q => R) | Q => (P => R)
-- t | t | t |    t          |    t
-- t | t | f |    f          |    f
-- t | f | t |    t          |    t
-- f | t | t |    t          |    t
-- f | f | t |    t          |    t
-- f | t | f |    t          |    t
-- t | f | f |    t          |    t
-- f | f | f |    t          |    t

-- 5. Yes
-- P | Q | R | P => (Q => R) | (P => Q) => R
-- t | t | t |   t           |          t
-- t | t | f |   f           |          f
-- t | f | t |   t           |          t
-- f | t | t |   t           |          t
-- f | f | t |   t           |          t
-- f | t | f |   t           |          t
-- t | f | f |   t           |          t
-- f | f | f |   t           |          t

-- 6. Yes
-- P | Q | (P => Q) => P | P
-- t | t |          t    | t
-- t | f |          t    | t
-- f | t |          f    | f
-- f | f |          f    | f

-- 7. Yes
-- P | Q | R | P v Q => R | (P => R) ^ (Q => R)
-- t | t | t |       t    |          t
-- t | t | f |       f    |          f
-- t | f | t |       t    |          t
-- f | t | t |       t    |          t
-- f | f | t |       t    |          t
-- f | t | f |       f    |          f
-- t | f | f |       f    |          f
-- f | f | f |       t    |          t

-- Ex.2.21
-- P | Q | !P => !Q
-- t | t |    t
-- t | f |    t
-- f | t |    f
-- t | t |    t

-- Ex.2.22: between 2.14212 and 2.14214 lays 2.14213

-- Ex.2.23
-- 1.
-- Allx(Ax => (Bx => Cx))
-- (Ax => (Bx => Cx)
-- Ax          Bx => Cx
--             Bx    Cx

-- 2.
-- Ex(Ax ^ Bx)
-- Ax ^  Bx
-- Ax    Bx

-- 3.
-- ExAx ^ ExBx
-- Ax   ^   Bx
-- Ax      Bx

-- Ex.2.26
-- 1. ExeQEyeQ(x<y)
-- 2. AxeREyeR(x<y)
-- 3. AxeZEm,neN(x=m-n)

-- Ex.2.27
-- 1. Ax(xeQ => Em,n(meZ ^ neZ ^ n != 0 ^ x = m/n))
-- 2. Ax(xeF => Ay(yeD ^ Oxy => Bxy))

-- Ex.2.31
-- 1. ExeR(x^2+1=0)
-- 2. AxeNEyeN(y>x)
-- 3. !ExeN(x != 1 ^ x != 13 ^ x|13)
-- 4. !ExeN(a != x ^ x != 1 ^ x|a)
-- 5. AyeNExeN(Py ^ Px => x > y)

-- Ex.2.32
-- 1. Ax(Lxd)
-- 2. Ax(Ldx)
-- 3. Ax(Mx => M'x)
-- 4. Ex(Bx => !Fx)

-- Ex.2.33
-- 1. Ax(Dx ^ Bx => !B'x) [Dx = x is dog, Bx = x barks, B'x = x bites]
-- 2. Ex(Gx => !G'x) [Gx = x is gold, G'x = x glitters]
-- 3. AxAy(Fx ^ F'y => Fy) [Fx = x is Diana's friend, F'x = x is friend of Diana's friend]
-- 4. ...

-- Ex.2.34
-- 1. AxEy(Lxd ^ (Cy => !Lyd))
-- 2. AxEyEz(Mx ^ Wy ^ Wz => Axyz)

-- Ex.2.35
-- 1. Ex(Kx ^ Ay(Ky => y=x) ^ !Rx)
-- 2. Ex(Kx ^ Ay(Ky => y=x) ^ Az(Szx => Lzx))

-- Ex.2.36
-- 1. There exists a real number x for which holds that x^2=5
-- 2. There is no maximum natural number.
-- 3. For every natural number n, there is no natural number d for which holds that 1 < d < (2^n+1) and d|(2^n+1)
-- 4. For every natural number n, there is a natural number m for which holds that n < m and all other numbers are lesser or equal to n or greater or equal to m.

-- Ex.2.37
-- 1. a) no, b) no, c) no, d) no, e) no, f) no
-- 2. a) yes, b) yes, c) yes, d) yes, e) yes, f) yes
-- 3. a) yes, b) yes, c) yes, d) yes, e) yes, f) yes
-- 4. a) no, b) no, c) no, d) no, e) no, f) no
-- 5. a) yes, b) yes, c) yes, d) yes, e) yes, f) yes

-- Ex.2.38
-- 1. a) yes, b) yes, c) yes, d) yes, e) yes, f) yes
-- 2. a) yes, b) yes, c) yes, d) no, e) yes, f) yes
-- 3. a) yes, b) yes, c) yes, d) no, e) no, f) yes
-- 4. a) yes, b) yes, c) yes, d) no, e) no, f) no
-- 5. a) 

-- Ex.2.39: The <=> operator is the equivalence operator. So when two formulas have the same truth table, they are by definition equivalent.

-- Ex.2.41
-- 1. AxeR!(x^2=5)
-- 2. EneNEmeN!(n<m)
-- 3. EneN!EdeN!(1<d<(2^n+1)^d(2^n+1))
-- 4. EneNEmeN!(n<m^ApeN(p<nvm<p))

-- Ex.2.46
-- The formula !ExeAPx states that there is no element x in set A for which holds that P
-- The formula ExneAPx states that there is an x that is not an element of set A for which holds that P
-- These formulas are fundamentally different in the way that the first one says that there is no x as an element of A, but probably of another set, and the
-- second one says that x is not an element of A.
-- However, they are equivalent in truth values for any given value of Px and any x.

-- Assumed that Px is true:
-- If we take x as a member of set B (explicitly not of A): both formulas are true.
-- If we take x as a member of set A: both formulas are false.

-- Assumed that Px is false:
-- If x is not a member of A: both formulas are false
-- If x is is a member of A: both formulas are false

-- Therefore, they are equivalent.

-- Ex.2.47
-- ExneA!Px states that there is an x not a member of A for which holds that not Px
-- ExeA!Px states that there is a member of A for which holds that not Px
-- These formulas are not equivalent. If Px is false, and if x is a member of A, then the first formula is false and the second is true.

-- Ex.2.48

-- Ex.2.50

-- Ex.2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

-- Ex.2.52
parity :: [Bool] -> Bool
parity xs = mod (length (filter (\ x -> x) xs)) 2 == 0

-- Ex.2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)
