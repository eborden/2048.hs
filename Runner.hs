module Runner (main) where

import TwentyFortyEight
import Data.Time.Clock (diffUTCTime, getCurrentTime)

ui h = return ()

run = do
    board <- startBoard (buildBoard 4 4)
    (final, win) <- gameLoop [(board, 0)] (aiCommand 3) (ui)
    let score = currentScore final
    return (score, win)

runSync acc x
    | x > 0 = do
        score <- run
        runSync (score:acc) (x - 1) 
    | otherwise = return acc

main = do
    start <- getCurrentTime
    scores <- runSync [] 100
    end <- getCurrentTime
    putStrLn $ "average: " ++ (show $ (fromIntegral $ sum $ map (fst) scores) / (fromIntegral $ length scores))
    putStrLn $ "win: " ++ (show $ (fromIntegral $ length $ filter (snd) scores) / (fromIntegral $ length scores) * 100.0) ++ "%"
    putStrLn $ "in " ++ (show $ diffUTCTime end start) ++ " seconds"
    return ()
