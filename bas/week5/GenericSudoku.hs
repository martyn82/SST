module GenericSudoku
where
import Data.List;

type Row                = Int 
type Column             = Int 
type Value              = Int
type Grid               = [[Value]]
type BlockGroup 	= [([Row], [Column])]
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

showNode :: (Grid -> IO ()) -> Node -> IO()
showNode show = (showSudoku show). fst


showNodeAndDiff :: (Grid -> IO ()) -> ([Node], Difficulty) -> IO ()
showNodeAndDiff show (ns, d) = do showDiff d
                                  mapM_ (\x -> showNode show x)  ns

showSudoku show = show . sud2grid

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
                       
type Difficulty = (Int, Int)
showDiff :: Difficulty -> IO ()
showDiff (s,n) = do putStr $ "Estimated difficulty: " ++ (show (round (rating (s,n) * 100))) ++ "%"
                    putStrLn (" (" ++ show s ++ " / " ++ show n ++ ")")
             

solvable :: Constraint -> Bool
solvable (_,_,vs) = length vs == 1

extendDifficulty :: Node -> Difficulty -> Difficulty
extendDifficulty n d = addDiff d (difficulty n)

difficulty :: Node -> Difficulty
difficulty (_, cs) = (length $ filter solvable cs',
                               length $ filter (not.solvable) cs')
                      where cs' = filter (\(_,_,vs) -> length vs > 0) cs
                               

addDiff :: Difficulty -> Difficulty -> Difficulty
addDiff (a,b) (a',b') = (a + a', b + b')

rating :: Difficulty -> Float
rating (a,b) = (log (fromIntegral a / fromIntegral b)) / (-1 * (exp 1))
                            
search :: (Node -> [Node]) 
       -> (Node -> Bool) -> ([Node], Difficulty) -> ([Node], Difficulty)
search succ goal ([],d) = ([],d)
search succ goal ((x:xs), d)
  | goal x    = ((x : next), d')
  | otherwise = search succ goal ((succ x) ++ xs, newDiff)
    where newDiff   = addDiff d (difficulty x)
          (next, d') = search succ goal (xs, newDiff)
          
  

solveNs :: RuleSet -> [Node] -> ([Node], Difficulty)
solveNs rs ns = search (succNode rs) solved (ns, (0,0))

succNode :: RuleSet -> Node -> [Node]
succNode _ (s,[]) = []
succNode rs (s,p:ps) = extendNode rs (s,ps) p 

solveAndShow :: (Grid -> IO ()) -> RuleSet -> Grid -> IO ()
solveAndShow show rs gr = solveShowNs show rs (initNode gr rs)

solveShowNs :: (Grid -> IO ()) -> RuleSet -> [Node] -> IO()
solveShowNs show rs ns = showNodeAndDiff show (solveNs rs ns)


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

showRowNrc :: [Value] -> IO()
showRowNrc [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
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

showGridNrc :: Grid -> IO()
showGridNrc [as,bs,cs,ds,es,fs,gs,hs,is] =
 do showBaseRow
    showRowNrc as; 
    showSegment
    showSegmentedRow bs;
    showSegmentedRow cs;
    showBaseRow
    showSegmentedRow ds;
    showSegment
    showRowNrc es;
    showSegment
    showSegmentedRow fs;
    showBaseRow
    showSegmentedRow gs;
    showSegmentedRow hs;
    showSegment
    showRowNrc is; 
    showBaseRow
	
	
solveAndShowNormal = solveAndShow showGrid normal
solveAndShowNrc    = solveAndShow showGridNrc nrc	
	

-- Excercise 3
-- To test the solver and difficulty rating use one of the following statements:
-- solveAndShowNormal example1
-- solveAndShowNormal onlineExampleEasy
-- solveAndShowNormal onlineExampleMedium
-- solveAndShowNormal onlineExampleExtreme
-- solveAndShowNrc nrcExampleEasy
-- solveAndShowNrc nrcExampleMedium
-- solveAndShowNrc nrcExampleHard

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

onlineExampleEasy :: Grid
onlineExampleEasy = [[0,9,0,2,0,3,0,8,0],
                     [2,0,0,0,4,0,0,0,3],
                     [7,0,0,1,0,8,0,0,9],
                     [4,2,5,0,8,0,9,1,7],
                     [6,0,0,0,0,0,0,0,2],
                     [9,7,1,0,2,0,3,6,8],
                     [8,0,0,4,0,5,0,0,1],
                     [1,0,0,0,6,0,0,0,4],
                     [0,4,0,8,0,1,0,2,0]]
                     
onlineExampleMedium :: Grid
onlineExampleMedium = [[0,6,0,1,5,0,4,0,0],
                       [5,0,0,0,0,6,0,7,0],
                       [3,4,0,9,0,0,0,0,0],
                       [0,0,3,0,0,0,8,0,0],
                       [1,7,4,5,0,8,2,3,9],
                       [0,0,5,0,0,0,1,0,0],
                       [0,0,0,0,0,1,0,9,6],
                       [0,5,0,2,0,0,0,0,1],
                       [0,0,9,0,4,5,0,2,0]]
            
onlineExampleExtreme :: Grid
onlineExampleExtreme = [[0,0,9,2,0,0,0,7,0],
                        [0,0,0,0,3,0,0,8,2],
                        [0,0,0,0,0,5,0,3,0],
                        [0,2,0,3,0,0,0,0,1],
                        [0,0,7,5,0,8,4,0,0],
                        [9,0,0,0,0,4,0,2,0],
                        [0,6,0,8,0,0,0,0,0],
                        [8,4,0,0,5,0,0,0,0],
                        [0,9,0,0,0,6,8,0,0]]
                        

--http://www.dkmsoftware.com/sudoku/HyperSudoku.htm
nrcExampleEasy :: Grid
nrcExampleEasy = [[8,7,1,0,3,9,0,0,0],
                  [0,9,0,4,0,0,0,8,0],
                  [0,0,0,7,0,2,0,9,0],
                  [9,2,0,0,7,0,6,0,3],
                  [5,0,0,0,9,0,0,0,2],
                  [6,0,7,0,5,0,0,4,9],
                  [0,4,0,3,0,5,0,0,0],
                  [0,8,0,0,0,7,0,1,0],
                  [0,0,0,9,4,0,5,3,7]]

nrcExampleMedium :: Grid
nrcExampleMedium = [[0,0,6,0,2,0,3,0,0],
                    [3,8,0,1,0,0,0,0,0],
                    [0,2,0,0,9,0,0,5,6],
                    [7,0,0,5,0,1,0,9,0],
                    [0,0,2,0,0,0,5,0,0],
                    [0,5,0,2,0,4,0,0,7],
                    [8,9,0,0,1,0,0,7,0],
                    [0,0,0,0,0,9,0,8,5],
                    [0,0,5,0,7,0,9,0,0]]

               
nrcExampleHard :: Grid
nrcExampleHard =  [[8,2,0,0,0,5,0,0,0],
                   [0,0,0,7,3,0,0,0,0],
                   [7,0,0,0,0,0,5,0,0],
                   [0,0,8,0,0,7,0,6,0],
                   [0,3,0,9,6,8,0,7,0],
                   [0,7,0,3,0,0,4,0,0],
                   [0,0,6,0,0,0,0,0,9],
                   [0,0,0,0,7,3,0,0,0],
                   [0,0,0,5,0,0,0,4,3]]

