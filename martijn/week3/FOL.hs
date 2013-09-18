module FOL
where

import Week3
import Techniques

-- generates a random FOL formula
getRandomFOLForm :: Int -> IO Formula
getRandomFOLForm 0 = do m <- getRandomInt 20
                        return (Atom (show (m+1)) [])
getRandomFOLForm d = do n <- getRandomInt 7
                        case n of
                            0 -> do m <- getRandomInt 20
                                    return (Atom (show (m+1)) [])
                            1 -> do f <- getRandomFOLForm (d-1)
                                    return (Neg f)
                            2 -> do m <- getRandomInt 4
                                    fs <- getRandomFOLForms (d-1) (m+1)
                                    return (Conj fs)
                            3 -> do m <- getRandomInt 4
                                    fs <- getRandomFOLForms (d-1) (m+1)
                                    return (Disj fs)
                            4 -> do f1 <- getRandomFOLForm (d-1)
                                    f2 <- getRandomFOLForm (d-1)
                                    return (Impl f1 f2)
                            5 -> do f1 <- getRandomFOLForm (d-1)
                                    f2 <- getRandomFOLForm (d-1)
                                    return (Equi f1 f2)
                            6 -> do m <- getRandomInt 20
                                    f <- getRandomFOLForm (d-1)
                                    return (Forall (show (m+1)) f)
                            7 -> do m <- getRandomInt 20
                                    f <- getRandomFOLForm (d-1)
                                    return (Exists (show (m+1)) f)

-- generates random FOL formulas
getRandomFOLForms :: Int -> Int -> IO [Formula]
getRandomFOLForms _ 0 = return []
getRandomFOLForms d n = do
                      f <- getRandomFOLForm d
                      fs <- getRandomFOLForms d (n-1)
                      return (f:fs)

-- generates n random FOL formulas
getRandomFOLFs :: Int -> IO [Formula]
getRandomFOLFs n = do d <- getRandomInt 3
                      getRandomFOLForms d n

