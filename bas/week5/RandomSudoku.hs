{---------------------------------------------------------

  Jan van Eijck, October 2010, modified October 2012

 --------------------------------------------------------}

module RandomSudoku where 

import Data.List
import GenericSudoku
import System.Random

emptyN :: RuleSet -> Node
emptyN rs = (\ _ -> 0, constraints (\ _ -> 0) rs)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- pick a random member from a list
-- empty list indicates failure
getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = 
  do n <- getRandomInt maxi
     return [xs !! n]
     where maxi = length xs - 1

-- randomize a list
randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)                     

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

extendN _ _  [] _ = []
extendN s cs xs rs = (extendNode rs (s,cs\\xs) (head xs))
        
rsuccNode (s,cs) rs = do xs <- getRandomCnstr cs
                         return $ extendN s cs xs rs

-- find a random solution for a member of 
rsolveNs :: RuleSet -> [Node] -> IO [Node]
rsolveNs rs ns = rsearch rs rsuccNode solved (return ns)

rsearch :: RuleSet -> (node -> RuleSet -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch rs succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch rs succ goal (succ (head xs) rs)
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else rsearch rs succ goal (return $ tail xs)
          
genRandomSudoku :: RuleSet -> IO Node
genRandomSudoku rs = do [r] <- rsolveNs rs [emptyN rs]
                        return r

randomS rs = genRandomSudoku rs >>= showNode

uniqueSol :: RuleSet -> Node -> Bool
uniqueSol rs node = singleton (solveNs rs [node]) where 
  singleton ([],_) = False
  singleton ([x],_)   = True
  singleton ((x:y:zs),_) = False


-- erase a position from a Sudoku
eraseS :: RuleSet -> Sudoku -> (Row,Column) -> Sudoku
eraseS rs s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

-- erase a position from a Node 
eraseN :: RuleSet -> Node -> (Row,Column) -> Node
eraseN rs n (r,c) = (s, constraints s rs) 
  where s = eraseS rs (fst n) (r,c) 

-- return a "minimal" node with a unique solution
-- by erasing positions until the result becomes ambiguous
minimalize :: RuleSet -> Node -> [(Row,Column)] -> Node
minimalize rs n [] = n
minimalize rs n ((r,c):rcs) | uniqueSol rs n' = minimalize rs n' rcs
                            | otherwise    = minimalize rs n  rcs
  where n' = eraseN rs n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genProblem :: RuleSet -> Node -> IO Node
genProblem rs n = do ys <- randomize xs
                     return (minimalize rs n ys)
   where xs = filledPositions (fst n)

main :: RuleSet -> IO ()
main rs = do [r] <- rsolveNs rs [emptyN rs]
             showNode r
             s  <- genProblem rs r
             showNode s
             solveShowNs rs [s]

             