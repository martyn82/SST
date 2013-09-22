module TestCNF
where

import Techniques

import Week2
import Logic
import CNF

-- function to determine whether the CNF property holds:
-- a CNF formula f' of formula f is correct iff f' and f are equivalent
cnfprop :: Form -> Bool
cnfprop f = equiv f (toCnf f)

-- tests a list of formulas
testc :: Int -> (Form -> Bool) -> [Form] -> IO ()
testc n prop []     = print (show n ++ " tests passed")
testc n prop (f:fs) =
        if prop f then do
           print ("pass on: " ++ show f ++ " == " ++ (show (prop f)))
           testc n prop fs
        else error ("failed test on: " ++ show f ++ " /= " ++ (show (prop f)))

-- tests CNF formulas
testcnf :: Int -> IO ()
testcnf n = do
        fs <- getRandomFs n
        testc n cnfprop fs

