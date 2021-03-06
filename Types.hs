module Types
    ( Tile
    , Score
    , Row
    , BoardPosition
    , Board
    , World
    , History
    , currentBoard
    , currentScore
    , Command(North, South, East, West, Quit, Restart, NoCommand)
    , Space
    , Monotonicity
    , Weight
    , AIScore
    , command
    , score
    , monotonicity
    , space
    , weight
    , maxBoard
    ) where


type Tile = Int
type Score = Int
type Row = [Tile]
type BoardPosition = (Int, Int)
type Board = [Row]
type World = (Board, Score)
type History = [World]

data Command = North | South | East | West | Quit | Restart | NoCommand deriving (Eq)

type Space = Int
type Monotonicity = Int
type Weight = Int
type MaxOnBoard = Int
type AIScore = (Command, Score, Monotonicity, Space, Weight, MaxOnBoard)

-- AIScore getters
command      (x, _, _, _, _, _) = x
score        (_, x, _, _, _, _) = x
monotonicity (_, _, x, _, _, _) = x
space        (_, _, _, x, _, _) = x
weight       (_, _, _, _, x, _) = x
maxBoard     (_, _, _, _, _, x) = x

-- History functions
currentBoard :: History -> Board
currentBoard = fst . head

currentScore :: History -> Score
currentScore = snd . head
