module Random where

import Techniques

-- generates an infinite list of random integers in the range of 0-10
genIntList :: IO [Int]
genIntList = sequence $ stream
                 where stream = getRandomInt 10 : stream

-- takes n items from an infinite list of random integers in the range of 0-10
genIntList' :: Int -> IO [Int]
genIntList' n = sequence $ (take n stream)
                   where stream = getRandomInt 10 : stream
