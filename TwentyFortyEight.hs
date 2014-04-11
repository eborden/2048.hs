module TwentyFortyEight
    ( gameLoop
    , buildBoard
    , startBoard
    , currentBoard
    , currentScore
    , aiCommand
    , randomCommand
    , Command(North, South, East, West, Quit, Restart, NoCommand)
    , History
    , World
    ) where

import Data.List (transpose, elem, elemIndices, intersperse, (\\), sortBy)
import Control.Applicative
import System.Random (randomRIO)
import Data.Function
import Data.Monoid
import Control.Concurrent

type Tile = Int
type Score = Int
type Row = [Tile]
type BoardPosition = (Int, Int)
type Board = [Row]
type World = (Board, Score)
type History = [World]
data Command = North | South | East | West | Quit | Restart | NoCommand deriving (Show, Eq)

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
emptyCells :: Board -> [(Int, Int)]
emptyCells = concat . foldl (\a r -> a ++ [zip (repeat $ length a) (emptyRowCells r)]) []

emptyRowCells :: Row -> [Int]
emptyRowCells = elemIndices 0

-- Key movements
moveBoard :: Board -> Command -> Board
moveBoard b x = case x of
    North -> shiftUp b
    South -> shiftDown b
    East -> shiftRight b
    West -> shiftLeft b
    _   -> b

shiftLeft :: Board -> Board
shiftLeft  = map (shiftRowLeft)
shiftRight = map (reverse . shiftRowLeft . reverse)
shiftUp    = transpose . shiftLeft . transpose
shiftDown  = transpose . shiftRight . transpose

-- Move all empty cells to the end and sum any matching cells
shiftRowLeft :: Row -> Row
shiftRowLeft = shift
    where 
        shift []     = []
        shift [x] = [x]
        shift (x:y:xs)
            | x > 0 = if x == y then ((x + y):(shift xs)) ++ [0]
                      else if y == 0 then (shift (x:xs)) ++ [0]
                      else x:(shift (y:xs))
            | x == 0 = (shift (y:xs)) ++ [0]

-- Find the total point value generated between board states
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
                let newBoard = moveBoard currBoard command
                    newScore = diffScore (currentScore h2) currBoard newBoard
                
                -- Keep the world turning
                return =<< gameLoop ((newBoard, newScore):h2) (getClientInput) clientAction


-- Find all possible moves
possibleMoves :: Board -> [Command]
possibleMoves b = filter (\x -> (moveBoard b x) /= b) [North, South, East, West]

-- Random command generator
randomCommand :: History -> IO Command
randomCommand h = do
    threadDelay 50000
    pickRand $ possibleMoves $ currentBoard h

-- AI powered command generator
type Space = Int
type Monotonicity = Int
type Contigeous = Int
type AIScore = (Command, Score, Monotonicity, Space, Contigeous, Int)

command      (x, _, _, _, _, _) = x
score        (_, x, _, _, _, _) = x
monotonicity (_, _, x, _, _, _) = x
space        (_, _, _, x, _, _) = x
contigeous   (_, _, _, _, x, _) = x
maxBoard     (_, _, _, _, _, x) = x
 
aiCommand :: Int -> History -> IO Command
aiCommand depth h = do
    let score = moveTree (head h) depth NoCommand
    return $ command score

moveTree :: World -> Int -> Command -> AIScore
moveTree w d c
    -- Iterate through all subsequent moves to see if command is successful
    | d > 0 && moveCount > 0   = bestCommand $ map (\command -> do
                                    let newBoard = moveBoard board command
                                    moveTree ( worstBoard newBoard
                                             , diffScore score board newBoard
                                             ) (d - 1) (if c == NoCommand then command else c)
                                 ) moves
    -- If there are no possible moves then return the score and command
    | d == 0 || moveCount == 0 = (c, score, monotonic board, spaceScore board, contigeousScore board, maxOnBoard board)

    where board = fst w
          moves = possibleMoves board
          moveCount = length moves
          score = snd w

worstBoard :: Board -> Board
worstBoard b = head $ sortBy (compare `on` (length . possibleMoves)) boards
    where boards = map (\x -> mutateBoard b x 2) (emptyCells b)

bestCommand :: [AIScore] -> AIScore
bestCommand x = head $ sortBy (heuristicSort) x

heuristicSort = flip compare `on` heuristicSum

heuristicSum :: AIScore -> Score
heuristicSum x = max 0 $ min (score x) $ fromEnum ((log sp * s) + (log mx * log s) + (c * m))
    where s = fromIntegral $ score x
          m = fromIntegral $ monotonicity x
          sp = fromIntegral $ space x
          c = fromIntegral $ contigeous x
          mx = fromIntegral $ maxBoard x

monotonic :: Board -> Int
monotonic b = m left + m down
    where m = monotonicityList
          left = b
          down = transpose left

monotonicityList :: Ord a => [a] -> Int
monotonicityList xs = ml xs
    where ml [x, y]   = (m x y)
          ml (x:y:xs) = (m x y) + ml (y:xs)
          m x y = if (x <= y) then 1 else -1

spaceScore :: Board -> Space
spaceScore = length . emptyCells

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
