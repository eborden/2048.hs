module Assert (main) where

import Types
import Heuristics
import Data.List (transpose)

ideal = [[8, 8, 4, 4]
        ,[8, 8, 4, 2]
        ,[8, 4, 2, 0]
        ,[4, 0, 0, 0]]

mono =  [[10, 9, 8, 7]
        ,[9, 8, 7, 6]
        ,[8, 7, 6, 5]
        ,[7, 6, 5, 0]]

bad =   [[2, 1, 8, 1]
        ,[1, 7, 1, 6]
        ,[9, 1, 2, 1]
        ,[1, 4, 1, 0]]

moot =  [[10, 10, 10, 10]
        ,[10, 10, 10, 10]
        ,[10, 10, 10, 10]
        ,[10, 10, 0, 0]]


testBoard name board = do
    putStrLn ""
    putStrLn "--------------------------------"
    putStrLn $ "|" ++ name
    putStrLn "monotonicity"
    putMonotonic board
    putMonotonic . transpose $ board
    putMonotonic . map (reverse) $ board
    putMonotonic . transpose . map (reverse) $ board
    
    putStrLn ""
    putStrLn "contigeous"
    putContigeous board
    putContigeous . transpose $ board
    putContigeous . map reverse $ board
    putContigeous . transpose . map (reverse) $ board
    
    putStrLn ""
    putStrLn "score"
    putScore board
    putScore . transpose $ board
    putScore . map reverse $ board
    putScore . transpose . map (reverse) $ board

    where putMonotonic = putStr . (" - " ++) . show . monotonic
          putContigeous = putStr . (" - " ++) . show . contigeousScore
          putScore = (\x -> putStr . (" - " ++) . show . heuristicSum $ heuristic NoCommand 100 x)

main = do
    testBoard "ideal" ideal
    testBoard "mono" mono
    testBoard "bad" bad
    testBoard "moot" moot
