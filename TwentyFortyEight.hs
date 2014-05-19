module TwentyFortyEight
    ( gameLoop
    , buildBoard
    , startBoard
    , addRandomCell
    , currentBoard
    , currentScore
    , aiCommand
    , Command(North, South, East, West, Quit, Restart, NoCommand)
    , History
    , World
    ) where

import Types
import Heuristics
import Data.List (transpose, sortBy)
import System.Random (randomRIO)
import Data.Function
import Control.Monad.Writer

-- Create a new board
buildBoard :: Int -> Int -> Board
buildBoard x y = replicate y (replicate x 0)

startBoard :: Board -> IO Board
startBoard b = addRandomCell b [2] 

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

-- Key movements
moveBoard :: Board -> Command -> (Board, Sum Int)
moveBoard b x = runWriter $ case x of
    North -> shiftUp b
    South -> shiftDown b
    East -> shiftRight b
    West -> shiftLeft b
    _   -> return b

shiftLeft :: Board -> Writer (Sum Int) Board
shiftLeft  = mapM (shiftRowLeft)
shiftRight = mapM (shiftRowLeft . reverse >=> return . reverse)
shiftUp = shiftLeft . transpose >=> return . transpose
shiftDown = shiftRight . transpose >=> return . transpose

-- Move all empty cells to the end and sum any matching cells
shiftRowLeft :: Row -> Writer (Sum Int) Row
shiftRowLeft []  = return []
shiftRowLeft [x] = return [x]
shiftRowLeft (x:y:xs)
            | x > 0 = if x == y then do 
                          let newValue = x + y
                          tell $ Sum newValue
                          zs <- shiftRowLeft xs
                          return $ (newValue:zs) ++ [0]
                      else if y == 0 then do
                          zs <- shiftRowLeft (x:xs)
                          return $ zs ++ [0]
                      else do
                          zs <- shiftRowLeft (y:xs)
                          return $ x:zs
            | x == 0 = do
                      zs <- shiftRowLeft (y:xs)
                      return $ zs ++ [0]

-- Conditions
gameOver :: Board -> Bool
gameOver b = (0 == (length $ emptyCells b)) && (0 == solutionCount b)

win :: Board -> Bool
win b = any (==True) $ map (elem 2048) b

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
addRandomCell :: Board -> [Tile] -> IO Board
addRandomCell b t = do
    rando <- pickRand $ emptyCells b
    tile <- pickRand t
    return $ mutateBoard b rando tile

-- Get a random list element
pickRand :: [a] -> IO a
pickRand xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

-- Recursive function that runs the whole damn show
gameLoop :: History
    -> (History -> IO Command)
    -> (History -> IO ())
    -> IO (History, Bool)
gameLoop h getClientInput clientAction = do
    -- Add a random cell if the world has changed
    h2 <- if worldHasChanged h then do
              currBoard <- addRandomCell (currentBoard h) (if (currentScore h) > 500 then [2, 2, 4] else [2])
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
        command <- getClientInput h2
        case command of
            Quit -> return (h2, False)
            _    -> do
                --Handle input
                let (newBoard, sumScore) = moveBoard currBoard command
                    newScore = (getSum sumScore) + (currentScore h2)
                
                -- Keep the world turning
                return =<< gameLoop ((newBoard, newScore):h2) (getClientInput) clientAction


{------------------------------|
             AI
-------------------------------}

-- Find all possible moves
possibleMoves :: Board -> [Command]
possibleMoves b = filter ((/= b) . fst . moveBoard b) [North, South, East, West]
 
-- AI powered command generator
aiCommand :: Int -> History -> IO Command
aiCommand depth h = do
    let score = moveTree (head h) depth NoCommand
    return $ command score

moveTree :: World -> Int -> Command -> AIScore
moveTree w depth c
    -- If there are no possible moves then return the score and command
    | depth == 0 && moveCount == 0 = heuristic c score board
    | depth == 0 = heuristic c score board
    | moveCount == 0 && win board = (c, 100000, 100000, 10000, 1, 100000)
    | moveCount == 0 = (c, 0, 0, 0, 0, 0)
    
    -- Iterate through all subsequent moves to see if the command is successful
    | depth > 0 = bestCommand $ do
            -- get all the possible boards and prune the worst
            let boards = pruneBoards $ map (makeMove) moves

            -- continue to recurse through the move tree
            map (nextMoves) boards

    where board = fst w
          moves = possibleMoves board
          moveCount = length moves
          score = snd w
          makeMove command = let (newBoard, moveScore) = moveBoard board command in
              (command, (newBoard, getSum moveScore + score))
          nextMoves (command, (newBoard, newScore)) = let world = (worstBoard depth newBoard, newScore) in
              moveTree world (depth - 1) (if c == NoCommand then command else c)

-- Sort boards by their heuristic score
pruneBoards :: [(Command, (Board, Score))] -> [(Command, (Board, Score))]
pruneBoards bs = take prune $ sortBy (flip compare `on` (heuristicSum . (\(c, (board, score)) -> heuristic c score board))) bs
    where prune = if length bs > 1 then length bs - 1 else 2

worstBoard :: Int -> Board -> Board
worstBoard depth b = head $ sortBy (compare `on` (length . possibleMoves)) boards
    where boards = map (\x -> mutateBoard b x (if (mod depth 3 > 1) then 4 else 2)) (emptyCells b)

bestCommand :: [AIScore] -> AIScore
bestCommand x = head $ sortBy (heuristicSort) x
