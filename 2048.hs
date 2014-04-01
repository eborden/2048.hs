module TwentyFortyEight () where

import Data.List (transpose)

type Tile = Integer
type Row = [Tile]
type Board = [Row]
type BoardPosition = (Integer, Integer)

board :: Board
board = [ [0, 0, 0, 0]
        , [0, 0, 0, 0]
        , [0, 0, 0, 0]
        , [0, 0, 0, 0]
        ]

-- Change a tile on the board
mutateBoard :: Board -> BoardPosition -> Tile -> Board
mutateBoard (x:xs) (r, c) t
    | r > 0     = x:mutateBoard xs (r - 1, c) t
    | otherwise = mutateRow x c t:xs

-- Change a tile in a row
mutateRow :: Row -> Integer -> Tile -> Row
mutateRow (x:xs) p t
    | p > 0     = x:mutateRow xs (p - 1) t
    | otherwise = t:xs

-- Key movements
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

-- Print the board in a semi friendly manner
printBoard b = mapM (putStrLn) $ map show b

main = do
    let board2 = mutateBoard board (1, 1) 2
    let board3 = mutateBoard board2 (1, 3) 2
    let board4 = mutateBoard board3 (1, 2) 2
    printBoard board
    putStrLn ""
    printBoard board2
    putStrLn ""
    printBoard $ shiftLeft board2
    putStrLn ""
    printBoard board3
    putStrLn ""
    printBoard $ shiftLeft board3
    putStrLn ""
    printBoard $ shiftRight board3
    putStrLn ""
    printBoard $ shiftUp board3
    putStrLn ""
    printBoard $ shiftDown board3
    putStrLn ""
    printBoard $ shiftLeft board4
    putStrLn ""
