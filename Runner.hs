module Runner (main) where

import TwentyFortyEight

ui h = return ()

loop :: Int -> [Int] -> IO [Int]
loop i acc = do 
    (final, win) <- gameLoop [(startBoard (2, 2) (buildBoard 4 4), 0)] (aiCommand 2) (ui)
    let score = currentScore final
    putStrLn $ show score
    putStrLn $ show win
    if i == 1 then return (score:acc)
    else loop (i - 1) (score:acc)

main = do
    scores <- loop 100 []
    putStrLn $ "average: " ++ (show $ (fromIntegral $ sum scores) / (fromIntegral $ length scores))
    return ()
