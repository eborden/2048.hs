module TwentyFortyEight () where

import Data.List (transpose, elemIndices, intersperse)
import System.Console.ANSI
import System.IO
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

-- Combine two strings and overwrite
overwrite :: [a] -> [a] -> [a]
overwrite [] _ = []
overwrite [x] _ = [x]
overwrite (x:xs) [] = x:overwrite xs []
overwrite (x:xs) [y] = y:overwrite xs []
overwrite (x:xs) (y:ys) = y:overwrite xs ys

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
    func (currentBoard, 0)

    -- Get key input
    c <- getChar

    let newBoard = keyPress currentBoard c
    
    -- Do key handling
    return =<< gameLoop ((newBoard, 0):(currentBoard, 0):(tail h)) func

-- Print the board in a semi friendly manner
printBoard = mapM (\x -> do
    printRow x
    putStr "\n")

printRow r = mapM (printCell) r

printCell c = colorByInt c $ overwrite "    " (show c :: String)

colorByInt x
    | x == 2 = colorStr Vivid Black Vivid White
    | x == 4 = colorStr Vivid Black Dull White
    | x == 8 = colorStr Vivid White Vivid Magenta
    | x == 16 = colorStr Vivid White Dull Magenta
    | x == 32 = colorStr Vivid White Vivid Blue
    | x == 64 = colorStr Vivid White Dull Blue
    | x == 128 = colorStr Vivid White Vivid Red
    | x == 256 = colorStr Vivid Black Vivid Yellow
    | x == 512 = colorStr Vivid Black Dull Yellow
    | x == 1024 = colorStr Vivid White Vivid Green
    | x == 2048 = colorStr Vivid White Vivid Cyan
    | otherwise = putStr

colorStr :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStr fgi fg bgi bg str = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
  putStr str
  setSGR []

-- Where the magic happens
main = do
    -- Turn input buffering off so key presses don't need an enter
    hSetBuffering stdin NoBuffering

    hideCursor

    -- Kick off the game loop with a fresh history
    gameLoop [(startBoard (2, 2) (buildBoard 4 4), 0)] (\(board, score) -> do
        -- Display the world
        clearScreen
        setCursorPosition 0 0
        putStrLn "2048.hs"
        putStrLn ""
        putStrLn $ "Score: " ++ (show $ sumBoard board)
        putStrLn ""
        printBoard board
        )
    
    return ()
