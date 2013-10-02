module Lab5

where

import Week5;
import Data.List;


mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt [x] = [x]
mergeSrt (x:xs) = merge [x] (mergeSrt xs) 

-- Excercise 1
-- Find a suitable assertion, and write an assertive version of this.
-- Deliverables: Assertion, Haskell program that uses this assertion, indication of time
-- spent

intersectYieldsItself :: (Ord a) => [a] -> [a] -> Bool
intersectYieldsItself a b = intersect a b == a

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = post1   sorted "output should be sorted" 
          $ assert1 intersectYieldsItself "output should contain all elements from the input" 
          $ mergeSrt

          
-- Excercise 2
-- Another approach to merge sort is to start by splitting the list to be sorted in equal
-- parts, recursively sort the parts, next merge.
-- Implement this, using the following split function.

-- Next, find a suitable assertion, and write an assertive version.
-- Deliverables: Haskell program, assertion, assertive version of Haskell program that
-- uses this assertion, indication of time spent.

split :: [a] -> ([a],[a])
split xs = let
            n = (length xs) `div` 2
            in
            (take n xs, drop n xs)
            
            
            
mergeSrt' :: (Ord a) => [a] -> [a]
mergeSrt' []  = []
mergeSrt' [x] = [x]
mergeSrt' x   = merge (mergeSrtA' a) (mergeSrtA' b)
                where (a,b) = split x
                
                
mergeSrtA' :: Ord a => [a] -> [a]
mergeSrtA' = post1   sorted "output should be sorted" 
          $ assert1 intersectYieldsItself "output should contain all elements from the input" 
          $ mergeSrt'
 
-- Excercise 3
-- The goal of this exercise is to extend the sudoku program from the course notes
-- with functions that can also handle sudokus of a special kind: the sudokus that
-- appear in NRC-Handelsblad each week (designed by Peter Ritmeester, from Oct
-- 8, 2005 onward). These NRC sudokus are special in that they have to satisfy a
-- few extra constraints: in addition to the usual sudoku constraints, each of the
-- 3x3 subgrids with left-top corner (2,2), (2,6), (6,2), and (6,6) should also yield a
-- surjective function.
-- Here is an example (the sudoku exercise of Saturday Nov 26, 2005)

--rewrote the provided example to allow different block setups

type Row                = Int 
type Column             = Int 
type Value              = Int
type Grid               = [[Value]]
type BlockGroup = [([Row], [Column])]
type RuleSet    = [BlockGroup]

values    = [1..9]
positions = [1..9]

--Setting up constraint types
rows,cols, blocks, nrcBlocks :: BlockGroup
rows = [([i],[1..9]) | i <- [1..9]]
cols = [([1..9],[i]) | i <- [1..9]]
blocks    = [(a,b) | a <- blocks', b <- blocks']
                where blocks' = [[1..3],[4..6],[7..9]]
nrcBlocks = [(a,b) | a <- nrcBlocks', b <- nrcBlocks']
                where nrcBlocks' = [[2..4],[6..8]]

--grouping constraint types into sudoku types
normal, nrc :: RuleSet
normal = [rows, cols, blocks]
nrc    = (nrcBlocks : normal)
                
type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)
  
  
block :: [[Int]] -> Int -> [Int]
block a x = concat $ filter (elem x) a

possibleBlocks :: BlockGroup -> (Row, Column) -> BlockGroup
possibleBlocks grp (r,c) = filter (\(rs, cs) -> elem r rs && elem c cs) grp



subGrid :: Sudoku -> BlockGroup -> (Row,Column) -> [Value]
subGrid s grp (r,c) =  [ s (r',c') | r' <- concat rs, c' <- concat cs]  
                where (rs, cs)  = unzip $ possibleBlocks grp (r,c)
                      
                
freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInSubgrid :: Sudoku -> BlockGroup -> (Row,Column) -> [Value]
freeInSubgrid s grp (r,c) = freeInSeq (subGrid s grp (r,c))

freeAtPos :: Sudoku -> RuleSet -> (Row,Column) -> [Value]
freeAtPos s setup (r,c) = foldr (\grp x -> intersect x (freeInSubgrid s grp (r,c))) values setup

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

subgridInjective s grp (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s grp (r,c))

consistentForCategory :: Sudoku -> BlockGroup -> Bool
consistentForCategory s grp = and $ [ subgridInjective s grp (r,c) | r <- rowFirsts, c <- colFirsts]
                                where rowFirsts    = map head rs
                                      colFirsts    = map head cs
                                      (rs, cs) = unzip grp 
   
consistent :: Sudoku -> RuleSet -> Bool
consistent s setup = and $ map (\grp -> consistentForCategory s grp) setup


extend :: Sudoku -> (Row,Column,Value) -> Sudoku
extend s (r,c,v) (i,j) | (i,j) == (r,c) = v
                       | otherwise      = s (i,j)

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

showSudoku = showGrid . sud2grid

solved  :: Node -> Bool
solved = null . snd

extendNode :: RuleSet -> Node -> Constraint -> [Node]
extendNode rs (s,constraints) (r,c,vs) = 
   [(extend s (r,c,v),
     sortBy length3rd $ 
         prune rs (r,c,v) constraints) | v <- vs ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = 
  compare (length zs) (length zs')
  
sameblock :: BlockGroup -> (Row,Column) -> (Row,Column) -> Bool
sameblock grp (r,c) (x,y) | length a + length b == 0 = False
                          | otherwise                = a == b
                    where a = possibleBlocks grp (r,c) 
                          b = possibleBlocks grp (x,y)
                
initNode :: Grid -> RuleSet -> [Node]
initNode gr rs = let s = grid2sud gr in 
                 if not $(consistent s rs) then [] 
                 else [(s, constraints s rs)]

anySameBlock :: RuleSet -> (Row,Column) -> (Row,Column) -> Bool
anySameBlock rs rc1 rc2 = or $ map (\grp -> sameblock grp rc1 rc2) rs
              
prune :: RuleSet -> (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _  _ [] = []
prune rs (r,c,v) ((x,y,zs):rest)
  | anySameBlock rs (r,c) (x,y) = (x,y,zs\\[v]) : prune rs (r,c,v) rest
  | otherwise                   = (x,y,zs)      : prune rs (r,c,v) rest
              
openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]
                            
constraints :: Sudoku -> RuleSet -> [Constraint] 
constraints s rs = sortBy length3rd 
    [(r,c, freeAtPos s rs (r,c)) | 
                       (r,c) <- openPositions s ]

                            
search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search succ goal [] = []
search succ goal (x:xs) 
  | goal x    = x : search succ goal xs
  | otherwise = search succ goal ((succ x) ++ xs)
  

solveNs :: RuleSet -> [Node] -> [Node]
solveNs rs = search (succNode rs) solved 

succNode :: RuleSet -> Node -> [Node]
succNode _ (s,[]) = []
succNode rs (s,p:ps) = extendNode rs (s,ps) p 

solveAndShow :: RuleSet -> Grid -> IO[()]
solveAndShow rs gr = solveShowNs rs (initNode gr rs)

solveShowNs :: RuleSet -> [Node] -> IO[()]
solveShowNs rs ns = sequence $ fmap showNode (solveNs rs ns)


showDgt :: Value -> String
showDgt 0 = " "
showDgt d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showDgt a1) ; putChar ' '
     putStr (showDgt a2) ; putChar ' '
     putStr (showDgt a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a4) ; putChar ' '
     putStr (showDgt a5) ; putChar ' '
     putStr (showDgt a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a7) ; putChar ' '
     putStr (showDgt a8) ; putChar ' '
     putStr (showDgt a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")


example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

                        
--http://www.hoeheethet.com/sudoku/sudoku.php
nrcExampleEasy :: Grid
nrcExampleEasy =  [[0,7,0,0,0,0,0,4,0],
                   [2,1,0,0,6,0,0,8,0],
                   [8,0,6,7,0,0,5,2,0],
                   [0,0,0,0,0,0,0,0,0],
                   [0,0,0,0,0,1,0,0,0],
                   [0,0,0,0,0,0,0,0,0],
                   [6,8,0,0,0,0,0,0,9],
                   [3,0,5,0,2,0,7,0,0],
                   [0,4,0,0,0,0,0,0,0]]

nrcExampleMedium :: Grid
nrcExampleMedium = [[0,0,6,0,7,8,2,3,0],
                    [0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,2,0,9,0],
                    [0,0,0,0,0,0,0,0,0],
                    [0,0,0,1,0,0,0,0,0],
                    [0,0,0,0,0,0,0,5,0],
                    [0,0,7,0,0,0,0,0,0],
                    [1,0,0,5,0,0,0,0,9],
                    [4,3,0,0,8,0,7,0,6]]
               
nrcExampleHard :: Grid
nrcExampleHard = [[0,7,0,0,0,0,0,4,0],
                  [2,1,0,0,6,0,0,8,0],
                  [8,0,6,7,0,0,5,2,0],
                  [0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,0,1,0,0,0],
                  [0,0,0,0,0,0,0,0,0],
                  [6,8,0,0,0,0,0,0,9],
                  [3,0,5,0,2,0,7,0,0],
                  [0,4,0,0,0,0,0,0,0]]





          
