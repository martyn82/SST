module Random
where

import Control.Monad
import System.Random

import Techniques
import Week3

listsize = 3
maxrand  = 3

-- generates a list of random integers
genIntList :: IO [Int]
genIntList = mapM (\ i -> getRandomInt maxrand) [1..listsize]

