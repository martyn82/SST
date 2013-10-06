module Lab5

where
import Data.List
import Week5
import RandomSudoku

-- Exercise 1 : Time spent 1 hour
mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

-- Elements of the orignal list have to be in the sorted list.
mergeSrtElementsProp :: Ord a => [a] -> [a] -> Bool
mergeSrtElementsProp xs ys = xs \\ ys == [] && ys \\ xs == []

-- Elements in the outpur are always sorted
mergeSrtSortedProp :: Ord a => [a] -> [a] -> Bool
mergeSrtSortedProp _ ys = sorted ys

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 mergeSrtSortedProp 
              $ assert1 mergeSrtElementsProp mergeSrt

-- Exercise 2 : Time spent 1 hour
split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2
           in (take n xs, drop n xs)

mergeSrt' :: Ord a => [a] -> [a]
mergeSrt' []  = []
mergeSrt' [x] = [x]
mergeSrt' xs  = let (x,y) = split xs
                in merge (mergeSrtA' x) (mergeSrtA' y) -- Auto testing of in between steps.

mergeSrtA' :: Ord a => [a] -> [a]
mergeSrtA' = assert1 mergeSrtSortedProp 
              $ assert1 mergeSrtElementsProp mergeSrt'

-- Exercise 3 : Time spent 4 hours
-- Formal statement of NRC constraint:
---- Pre and postcondition
---- Sudoku constraints +
---- For all 3x3 blocks with top-left at [(2,2), (2,6), (6,2), (6,6)];
----   For all non-empty values:
----     All values in block are different
----     All values in block are between 1 ... 9

nrcExample1 :: Grid
nrcExample1 = [[0,0,0,3,0,0,0,0,0],
               [0,0,0,7,0,0,3,0,0],
               [2,0,0,0,0,0,0,0,8],
               [0,0,6,0,0,5,0,0,0],
               [0,9,1,6,0,0,0,0,0],
               [3,0,0,0,7,1,2,0,0],
               [0,0,0,0,0,0,0,3,1],
               [0,8,0,0,4,0,0,0,0],
               [0,0,2,0,0,0,0,0,0]]

nrcSolution1 = solveAndShowNRC nrcExample1

-- Exercise 4: Time spent 1 hours
genRandomNRCSudoku :: IO Node
genRandomNRCSudoku = do r <- genRandomSudokuNRC
                        s <- genProblem r
                        showNode s
                        return s

solveRandomNRCSudoku = do s <- genRandomNRCSudoku
                          solveShowNsNRC [s]


-- Exercise 5: Time spent 2 hours
-- Minimal: Assume that when we remove a number from the sudoku, the sudoku becomes non-unique and that this non-uniqueness does not change when we remove another number from the new sudoku.
nrcSudokuIsMinimal :: Node -> Bool
nrcSudokuIsMinimal n = and [not $ uniqueSol $ eraseN n p | p <- xs]
                       where xs = filledPositions (fst n)

-- Minimal: Check if the sudoku yields a unique solution when a subset of the filled out numbers is used.
-- Sadly this does not terminate. I cannot proof however that removing only item is minimal. It might be that by removing two items, the puzzle becomes minimal again..
nrcSudokuIsMinimal2 :: Node -> Bool
nrcSudokuIsMinimal2 n = and [not $ uniqueSol $ removePositions n ps | ps <- xss]
                        where xs = filledPositions (fst n)
                              xss = (tail . subsequences) xs

removePositions :: Node -> [(Row,Column)] -> Node
removePositions n [] = n
removePositions n (x:xs) = removePositions (eraseN n x) xs

testNRCSudoku = testNRCSudokuProps [nrcSudokuIsMinimal]

testNRCSudokuProps :: [(Node -> Bool)] -> IO ()
testNRCSudokuProps ps = do s <- genRandomNRCSudoku
                           putStrLn ("To test:")
                           showNode s
                           if and [p s | p <- ps]
                           then print ("passed")
                           else error ("failed")
