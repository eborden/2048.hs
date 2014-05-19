module Heuristics
    ( monotonic
    , openSpace
    , heuristicSort
    , emptyCells
    , solutionCount
    , maxOnBoard
    , heuristic
    , heuristicSum
    ) where

import Types
import Data.List (transpose, elem, elemIndices, intersperse, (\\), foldl')
import Data.Function (on)

-- Get empty cell positions
emptyCells :: Board -> [(Int, Int)]
emptyCells = concat . foldl (\a r -> a ++ [zip (repeat $ length a) (emptyRowCells r)]) []
    where emptyRowCells = elemIndices 0

heuristic :: Command -> Score -> Board -> AIScore
heuristic c s b = (c, s, monotonic b, openSpace b, neighborWeight b, maxOnBoard b)

heuristicSort = flip compare `on` heuristicSum

-- Score from a collection of heuristics
heuristicSum :: AIScore -> Score
heuristicSum x = if (sp > 0) then round (s + (sp * m) - w) else -1000000000
    where s = fromIntegral $ score x
          m = fromIntegral $ monotonicity x
          sp = fromIntegral $ space x
          w = fromIntegral $ weight x
          mx = fromIntegral $ maxBoard x

monotonic :: Board -> Int
monotonic b = m left + m down
    where m = sum . (map (monotonicityList . filter (/= 0)))
          left = b
          down = transpose left

monotonicityList :: Ord a => [a] -> Int
monotonicityList xs = abs $ ml 0 xs
    where ml acc []       = acc
          ml acc [x]      = acc
          ml acc [x, y]   = acc + (monoScore x y)
          ml acc (x:y:xs) = ml (acc + monoScore x y) (y:xs)
          monoScore x y 
            | x >= y           = 1
            | otherwise        = -1

openSpace :: Board -> Space
openSpace = length . emptyCells

maxOnBoard :: Board -> Int
maxOnBoard b = maximum $ map (maximum) b

-- Heuristic summing the distance between neighboring cells
-- This scoring prefers high value tiles to neighor each other and 
-- amass near the edges of the board.
neighborWeight :: Board -> Int
neighborWeight b = left b + down b
    where sumWeight acc row = acc + rowWeight row
          left = foldl' (sumWeight) 0
          down    = left . transpose

rowWeight :: Row -> Int
rowWeight xs = abs $ sumRow 0 xs
    where sumRow acc [x, y, z]   = acc + sumNeighbors (Just x, Just y, Just z)
          sumRow acc [x, y]   = acc + (sumNeighbors (Just x, Just y, Nothing))
          sumRow acc (x:y:z:xs) = sumRow (acc + sumNeighbors (Just x, Just y, Just z)) (y:z:xs)
          sumNeighbors (Just x, Just y, Just z) = (abs $ y - x) + (abs $ y - z)
          sumNeighbors (Just x, Just y, Nothing) = (abs $ y - x)

-- Find if solutions are possible
solutionCount :: Board -> Int
solutionCount b = length $ filter (\x -> fst x == snd x) (allNeighbors b)

allNeighbors :: Board -> [(Int, Int)]
allNeighbors b = concat $ (map (rightNeighbors) b) ++ (map (rightNeighbors) (transpose b))

rightNeighbors :: Row -> [(Int, Int)]
rightNeighbors (x:y:xs) = (x,y):rightNeighbors (y:xs)
rightNeighbors _ = []
