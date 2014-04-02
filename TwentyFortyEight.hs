module TwentyFortyEight
    ( gameLoop
    , buildBoard
    , startBoard
    ) where

import Data.List (transpose, elemIndices, intersperse)
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

-- Find if solutions are possible
solutionCount :: Board -> Int
solutionCount b = length $ filter (\x -> fst x == snd x) (allNeighbors b)

allNeighbors :: Board -> [(Int, Int)]
allNeighbors b = concat $ (map (rightNeighbors) b) ++ (map (rightNeighbors) (reverse b))

rightNeighbors :: Row -> [(Int, Int)]
rightNeighbors (x:y:xs) = (x,y):rightNeighbors (y:xs)
rightNeighbors _ = []

--summableNeighbors :: Row -> Int -> Int
--summableNeighbors (x:y:xs) = 

{------------------------------|
       IO Business
-------------------------------}

-- Determine whether to append a new cell
addRandomCell :: History -> IO Board
addRandomCell ((present, _):(past, _):_)
    | present == past = do return present
    | present /= past = mutateRandomCell present
addRandomCell [(present, _), (past, _)]
    | present == past = do return present
    | present /= past = mutateRandomCell present
addRandomCell [(present, _)] = mutateRandomCell present

mutateRandomCell :: Board -> IO Board
mutateRandomCell b = do
    rando <- pickRand $ emptyCells b
    return $ mutateBoard b rando 2

-- Get a random list element
pickRand :: [a] -> IO a
pickRand xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

-- Recursive function that runs the whole damn show
gameLoop :: History -> (World -> IO b) -> IO History
gameLoop h func = do
    -- Add a random cell if the world has changed
    currentBoard <- addRandomCell h    

    -- Pass the current board to the client
    func (currentBoard, currentScore h)

    -- Collect game over conditions
    let full = 0 == (length $ emptyCells currentBoard)
        noSolutions = 0 == solutionCount currentBoard

    if (full && noSolutions) then
        return $ (currentBoard, 0):(tail h)
    else do
        -- Get key input
        c <- getChar

        let newBoard = keyPress currentBoard c
        
        -- Do key handling
        return =<< gameLoop ((newBoard, sumBoard newBoard):(currentBoard, currentScore h):(tail h)) func
