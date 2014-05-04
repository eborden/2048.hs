module Heuristics
    ( monotonic
    , openSpace
    , heuristicSort
    , emptyCells
    , solutionCount
    , maxOnBoard
    , heuristic
    , heuristicSum
    , rowWeight
    ) where

import Types
import Data.List (transpose, elem, elemIndices, intersperse, (\\))
import Data.Function

-- Get empty cell positions
emptyCells :: Board -> [(Int, Int)]
emptyCells = concat . foldl (\a r -> a ++ [zip (repeat $ length a) (emptyRowCells r)]) []
    where emptyRowCells = elemIndices 0

heuristic :: Command -> Score -> Board -> AIScore
heuristic c s b = (c, s, monotonic b, openSpace b, boardWeight b, maxOnBoard b)

heuristicSort = flip compare `on` heuristicSum

-- Score from a collection of heuristics
heuristicSum :: AIScore -> Score
heuristicSum x = fromEnum $ (s + (log sp * mx) + m * sp - c)
    where s = fromIntegral $ score x
          m = fromIntegral $ monotonicity x
          sp = fromIntegral $ space x
          c = fromIntegral $ weight x
          mx = fromIntegral $ maxBoard x

monotonic :: Board -> Int
monotonic b = m left + m down
    where m = sum . (map (monotonicityList))
          left = b
          down = transpose left

monotonicityList :: Row -> Int
monotonicityList xs = abs $ ml xs
    where ml []       = 0
          ml [x, y]   = (m x y)
          ml (x:y:xs) = (m x y) + ml (y:xs)
          m x y 
            | x == 0 || 0 == y = 0
            | x >= y           = 1
            | otherwise        = -1

openSpace :: Board -> Space
openSpace = length . emptyCells

maxOnBoard :: Board -> Int
maxOnBoard b = maximum $ map (maximum) b

boardWeight :: Board -> Int
boardWeight b = (sum (map (rowWeight) b)) + (sum (map (rowWeight) (transpose b)))

rowWeight :: Row -> Int
rowWeight xs = abs $ (ml xs) + (m (Just (xs !! 1), Just (xs !! 0), Nothing))
    where ml [x, y, z]   = (m (Just x, Just y, Just z))
          ml (x:y:z:xs) = (m (Just x, Just y, Just z)) + ml (y:z:xs)
          m (Just x, Just y, Just z) = (abs $ y - x) + (abs $ y - z)
          m (Just x, Just y, Nothing) = (abs $ y - x)

-- Find if solutions are possible
solutionCount :: Board -> Int
solutionCount b = length $ filter (\x -> fst x == snd x) (allNeighbors b)

allNeighbors :: Board -> [(Int, Int)]
allNeighbors b = concat $ (map (rightNeighbors) b) ++ (map (rightNeighbors) (transpose b))

rightNeighbors :: Row -> [(Int, Int)]
rightNeighbors (x:y:xs) = (x,y):rightNeighbors (y:xs)
rightNeighbors _ = []
