module TwentyFortyEight
    ( gameLoop
    , buildBoard
    , startBoard
    , currentBoard
    , currentScore
    ) where

import Data.List (transpose, elem, elemIndices, intersperse, (\\))
import Control.Applicative
import System.Random (randomRIO)

type Tile = Int
type Score = Int
type Row = [Tile]
type BoardPosition = (Int, Int)
type Board = [Row]
type World = (Board, Score)
type History = [World]

-- Create a new board
buildBoard :: Int -> Int -> Board
buildBoard x y = replicate y (replicate x 0)

startBoard :: BoardPosition -> Board -> Board
startBoard x b = mutateBoard b x 2 

-- History functions
currentBoard :: History -> Board
currentBoard = fst . head

currentScore :: History -> Score
currentScore = snd . head

-- Change a tile on the board
mutateBoard :: Board -> BoardPosition -> Tile -> Board
mutateBoard (x:xs) (r, c) t
    | r > 0     = x:mutateBoard xs (r - 1, c) t
    | otherwise = mutateRow x c t:xs

-- Change a tile in a row
mutateRow :: Row -> Int -> Tile -> Row
mutateRow (x:xs) p t
    | p > 0     = x:mutateRow xs (p - 1) t
    | otherwise = t:xs

-- Get empty cell positions
emptyCells b = concat $ foldl (\a r -> a ++ [zip (repeat $ length a) (emptyRowCells r)]) [] b

emptyRowCells :: Row -> [Int]
emptyRowCells r = elemIndices 0 r

-- Key movements
keyPress :: Board -> Char -> Board
keyPress b x = case x of
    'a' -> shiftLeft b
    'w' -> shiftUp b
    'd' -> shiftRight b
    's' -> shiftDown b
    _   -> b

shiftLeft :: Board -> Board
shiftLeft b = map (sumRowLeft . shiftRowLeft) $ b

shiftRight :: Board -> Board
shiftRight b = map (reverse . sumRowLeft . shiftRowLeft . reverse) $ b

shiftUp :: Board -> Board
shiftUp = transpose . shiftLeft . transpose

shiftDown :: Board -> Board
shiftDown = transpose . shiftRight . transpose

-- Move all empty cells to the end
shiftRowLeft :: Row -> Row
shiftRowLeft []     = []
shiftRowLeft (x:xs)
    | x > 0  = x:(shiftRowLeft xs)
    | x == 0 = (shiftRowLeft xs) ++ [0]

-- Sum all neighboring cells that are equal and replace them with a 0
sumRowLeft :: Row -> Row
sumRowLeft []     = []
sumRowLeft (x:[]) = [x]
sumRowLeft (x:y:xs)
    | x > 0 = if x == y
              then (x + y):(sumRowLeft (0:xs))
              else x:(sumRowLeft (y:xs))
    | x == 0 = (sumRowLeft (y:xs)) ++ [0]

-- Find the total point value on the board
sumBoard :: Board -> Int
sumBoard x = sum (map sum x)

diffScore :: Int -> Board -> Board -> Int
diffScore score old new = score + (sum $ (concat old) \\ (concat new))

-- Find if solutions are possible
solutionCount :: Board -> Int
solutionCount b = length $ filter (\x -> fst x == snd x) (allNeighbors b)

allNeighbors :: Board -> [(Int, Int)]
allNeighbors b = concat $ (map (rightNeighbors) b) ++ (map (rightNeighbors) (transpose b))

rightNeighbors :: Row -> [(Int, Int)]
rightNeighbors (x:y:xs) = (x,y):rightNeighbors (y:xs)
rightNeighbors _ = []

-- Conditions
gameOver :: Board -> Bool
gameOver b = (0 == (length $ emptyCells b)) && (0 == solutionCount b)

win :: Board -> Bool
win b = 1024 `elem` (concat b)

worldHasChanged :: History -> Bool
worldHasChanged ((present, _):(past, _):_)
    | present == past = False
    | present /= past = True
worldHasChanged [(present, _), (past, _)]
    | present == past = False
    | present /= past = True
worldHasChanged _ = True

{------------------------------|
       IO Business
-------------------------------}

-- Append random cell
addRandomCell :: Board -> IO Board
addRandomCell b = do
    rando <- pickRand $ emptyCells b
    return $ mutateBoard b rando 2

-- Get a random list element
pickRand :: [a] -> IO a
pickRand xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

-- Recursive function that runs the whole damn show
gameLoop :: History -> (History -> IO b) -> IO (History, Bool)
gameLoop h clientAction = do
    -- Add a random cell if the world has changed
    h2 <- if worldHasChanged h then do
              currBoard <- addRandomCell (currentBoard h)
              return ((currBoard, currentScore h):h)
          else do return h

    -- Pass history to client
    clientAction h2

    let currBoard = currentBoard h2
    
    if win currBoard then
        return (h2, True)
    else if gameOver currBoard then
        return (h2, False)
    else do
        c <- getChar
        if c == 'q' then
            return (h2, False)
        else do
            --Handle input
            let newBoard = keyPress currBoard c
                newScore = diffScore (currentScore h2) currBoard newBoard
            
            -- Keep the world turning
            return =<< gameLoop ((newBoard, newScore):h2) clientAction
