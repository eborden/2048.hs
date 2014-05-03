module Heuristics
    ( monotonic
    , openSpace
    , contigeousScore
    , heuristicSort
    , emptyCells
    , solutionCount
    , maxOnBoard
    , heuristic
    , heuristicSum
    ) where

import Types
import Data.List (transpose, elem, elemIndices, intersperse, (\\))
import Data.Function

-- Get empty cell positions
emptyCells :: Board -> [(Int, Int)]
emptyCells = concat . foldl (\a r -> a ++ [zip (repeat $ length a) (emptyRowCells r)]) []
    where emptyRowCells = elemIndices 0

heuristic :: Command -> Score -> Board -> AIScore
heuristic c s b = (c, s, monotonic b, openSpace b, contigeousScore b, maxOnBoard b)

heuristicSort = flip compare `on` heuristicSum

-- Score from a collection of heuristics
heuristicSum :: AIScore -> Score
heuristicSum x = minMax x (s + (log sp * mx) + (m + 1 * c) * sp)
    where s = fromIntegral $ score x
          m = fromIntegral $ monotonicity x
          sp = fromIntegral $ space x
          c = fromIntegral $ contigeous x
          mx = fromIntegral $ maxBoard x

minMax x algo = max 0 $ min (score x) $ fromEnum algo

monotonic :: Board -> Int
monotonic b = m left + m down
    where m = monotonicityList
          left = b
          down = transpose left

monotonicityList :: Ord a => [a] -> Int
monotonicityList xs = abs $ ml xs
    where ml [x, y]   = (m x y)
          ml (x:y:xs) = (m x y) + ml (y:xs)
          m x y = if (x >= y) then 1 else -1

openSpace :: Board -> Space
openSpace = length . emptyCells

maxOnBoard :: Board -> Int
maxOnBoard b = maximum $ map (maximum) b

contigeousScore :: Board -> Int
contigeousScore b = sum $ map (contigeousValues) b

contigeousValues :: Eq a => [a] -> Int
contigeousValues xs = distance 0 xs
    where distance s [x] = s
          distance s (x:y:xs)
              | x == y = distance (s + 1) (y:xs)
              | x /= y = distance s (y:xs)

-- Find if solutions are possible
solutionCount :: Board -> Int
solutionCount b = length $ filter (\x -> fst x == snd x) (allNeighbors b)

allNeighbors :: Board -> [(Int, Int)]
allNeighbors b = concat $ (map (rightNeighbors) b) ++ (map (rightNeighbors) (transpose b))

rightNeighbors :: Row -> [(Int, Int)]
rightNeighbors (x:y:xs) = (x,y):rightNeighbors (y:xs)
rightNeighbors _ = []
