-- The members of every subgrid2 [i,j] with i,j ranging over 2..4 and 6..8
-- should be all different.
-- I.e., the list of values
-- [f [i,j] | i <- [2..4], j <- [2..4] ]
-- should not have duplicates, and similarly for the other subgrids

module NRCSudoku
where

import Data.List;

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]



showDgt :: Value -> String
showDgt 0 = " "
showDgt d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showDgt a1) ; putStr "  "
     putStr (showDgt a2) ; putStr "  "
     putStr (showDgt a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a4) ; putStr "  "
     putStr (showDgt a5) ; putStr "  "
     putStr (showDgt a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a7) ; putStr "  "
     putStr (showDgt a8) ; putStr "  "
     putStr (showDgt a9) ; putChar ' '
     putChar '|'         ; putChar '\n'
     
showSegmentedRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showDgt a1) ; putChar ' '
     putChar '|'         
     putStr (showDgt a2) ; putStr "  "
     putStr (showDgt a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a4) ; putStr "| "
     putStr (showDgt a5) ; putStr " |"
     putStr (showDgt a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a7) ; putStr "  "
     putStr (showDgt a8) ; putStr "| "
     putStr (showDgt a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showBaseRow :: IO ()
showBaseRow = putStrLn ("+---------+---------+---------+")

showSegment :: IO ()
showSegment = putStrLn ("|   +-----|--+   +--|-----+   |")

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do showBaseRow
    showRow as; 
    showSegment
    showSegmentedRow bs;
    showSegmentedRow cs;
    showBaseRow
    showSegmentedRow ds;
    showSegment
    showRow es;
    showSegment
    showSegmentedRow fs;
    showBaseRow
    showSegmentedRow gs;
    showSegmentedRow hs;
    showSegment
    showRow is; 
    showBaseRow
    
type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

blocksNrc :: [[Int]]
blocksNrc = [[2..4],[6..8]] 

blNrc :: Int -> [Int]
blNrc x = concat $ filter (elem x) blocksNrc 

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]
  
subGridNrc :: Sudoku -> (Row,Column) -> [Value]
subGridNrc s (r,c) = 
  [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = 
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = 
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

freeInSubgridNrc :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNrc s (r,c) = freeInSeq (subGridNrc s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c))
   
freeAtPosNrc s (r,c) = freeAtPos s (r,c) 
   `intersect` (freeInSubgridNrc s (r,c)) 

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))
   
subgridNrcInjective :: Sudoku -> (Row,Column) -> Bool
subgridNrcInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGridNrc s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]

nrcConsistent s = consistent s && and [ subgridNrcInjective s (r,c) | r <- [2,6], c <- [2,6]]

extend :: Sudoku -> (Row,Column,Value) -> Sudoku
extend s (r,c,v) (i,j) | (i,j) == (r,c) = v
                       | otherwise      = s (i,j)

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = 
   [(extend s (r,c,v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]
         

extendNrcNode :: Node -> Constraint -> [Node]
extendNrcNode (s,constraintsNrc) (r,c,vs) = 
   [(extend s (r,c,v),
     sortBy length3rd $ 
         pruneNrc (r,c,v) constraintsNrc) | v <- vs ]


length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = 
  compare (length zs) (length zs')

prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _ _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) =  (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest  
  
prune :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune = prune' prune

pruneNrc :: (Row,Column,Value) -> [Constraint] -> [Constraint]
pruneNrc (r,c,v) ((x,y,zs):rest) | sameblockNrc (r,c) (x,y) =  (x,y,zs\\[v]) : prune' pruneNrc (r,c,v) rest
                                 | otherwise = prune' pruneNrc (r,c,v) ((x,y,zs):rest) 
  
  
sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = blNrc r == blNrc x && blNrc c == blNrc y 

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]
              
initNrcNode :: Grid -> [Node]
initNrcNode gr = let s = grid2sud gr in 
                 if (not . nrcConsistent) s then [] 
                 else [(s, constraintsNrc s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, freeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]
                       
constraintsNrc :: Sudoku -> [Constraint] 
constraintsNrc s = sortBy length3rd 
    [(r,c, freeAtPosNrc s (r,c)) | 
                       (r,c) <- openPositions s ]

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search succ goal [] = []
search succ goal (x:xs) 
  | goal x    = x : search succ goal xs
  | otherwise = search succ goal ((succ x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

solveNrcs :: [Node] -> [Node]
solveNrcs = search succNrcNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

succNrcNode :: Node -> [Node]
succNrcNode (s,[]) = []
succNrcNode (s,p:ps) = extendNrcNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveAndShowNrc :: Grid -> IO[()]
solveAndShowNrc gr = solveShowNrcs (initNrcNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs ns = sequence $ fmap showNode (solveNs ns)

solveShowNrcs :: [Node] -> IO[()]
solveShowNrcs ns = sequence $ fmap showNode (solveNrcs ns)

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

nrcExcercise3 :: Grid
nrcExcercise3 =  [[0,0,0,3,0,0,0,0,0],
                  [0,0,0,7,0,0,3,0,0],
                  [2,0,0,0,0,0,0,0,8],
                  [0,0,6,0,0,5,0,0,0],
                  [0,9,1,6,0,0,0,0,0],
                  [3,0,0,0,7,1,2,0,0],
                  [0,0,0,0,0,0,0,3,1],
                  [0,8,0,0,4,0,0,0,0],
                  [0,0,2,0,0,0,0,0,0]]
            
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