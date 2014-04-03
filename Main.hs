module Main (main) where

import TwentyFortyEight
import System.Console.ANSI
import System.IO

-- Combine two strings and overwrite
overwrite :: [a] -> [a] -> [a]
overwrite [] _ = []
overwrite [x] _ = [x]
overwrite (x:xs) [] = x:overwrite xs []
overwrite (x:xs) [y] = y:overwrite xs []
overwrite (x:xs) (y:ys) = y:overwrite xs ys

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
    (final, win) <- gameLoop [(startBoard (2, 2) (buildBoard 4 4), 0)] (\h -> do
        let score = currentScore h
            board = currentBoard h
        -- Display the world
        clearScreen
        setCursorPosition 0 0
        putStrLn "2048.hs"
        putStrLn ""
        putStrLn $ "Score: " ++ (show score)
        putStrLn ""
        printBoard board
        )

    if win then mapM (putStrLn) ["", "", "Congratulations!"]
    else mapM (putStrLn) ["", "", "Game Over. Press w to play again."]

    c <- getChar
    if c =='w' then main
    else showCursor
    
    clearScreen
    
    return ()
