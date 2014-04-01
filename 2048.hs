module TwentyFortyEight () where

import Data.List (transpose, elemIndices)
import System.Console.ANSI
import System.IO
import Control.Applicative
import System.Random (randomRIO)

type Tile = Int
type Row = [Tile]
type Board = [Row]
type BoardPosition = (Int, Int)

buildBoard :: Int -> Int -> Board
buildBoard x y = replicate y (replicate x 0)

startBoard :: BoardPosition -> Board -> Board
startBoard x b = mutateBoard b x 2 

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


{------------------------------|
       Impure IO Business
-------------------------------}

-- Get a random list element
pickRand :: [a] -> IO a
pickRand xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

-- Print the board in a semi friendly manner
printBoard b = mapM (putStrLn) $ map show b

gameLoop :: Board -> IO Board
gameLoop b = do
    -- Add random tile
    rando <- pickRand $ emptyCells b
    let b2 = mutateBoard b rando 2

    -- Display the world
    clearScreen
    putStrLn ""
    printBoard b2

    -- Do key handling
    return =<< gameLoop =<< keyPress b2 <$> getChar

main = do
    hSetBuffering stdin NoBuffering

    gameLoop $ startBoard (1, 2) (buildBoard 4 4)
    
    return ()
