module Assert (main) where

import Heuristics
import Data.List (transpose)

ideal = [[8, 8, 4, 4]
        ,[8, 8, 4, 2]
        ,[8, 4, 2, 0]
        ,[4, 0, 0, 0]]

mono =  [[10, 9, 8, 7]
        ,[9, 8, 7, 6]
        ,[8, 7, 6, 5]
        ,[7, 6, 5, 4]]

bad =   [[2, 1, 8, 1]
        ,[1, 7, 1, 6]
        ,[9, 1, 2, 1]
        ,[1, 4, 1, 3]]

moot =  [[1, 1, 1, 1]
        ,[1, 1, 1, 1]
        ,[1, 1, 1, 1]
        ,[1, 1, 1, 1]]


testBoard board = do
    putStrLn "monotonicity---------------"
    putMonotonic board
    putMonotonic . transpose $ board
    putMonotonic . map (reverse) $ board
    putMonotonic . transpose . map (reverse) $ board
    
    putStrLn "contigeous---------------"
    putContigeous board
    putContigeous . transpose $ board
    putContigeous . map reverse $ board
    putContigeous . transpose . map (reverse) $ board

    where putMonotonic = putStrLn . show . monotonic
          putContigeous = putStrLn . show . contigeousScore

main = do
    testBoard ideal
    testBoard mono
    testBoard bad
    testBoard moot
