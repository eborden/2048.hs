module Main (main) where

import System.Environment (getArgs)
import qualified Runner as R
import qualified UI as U

main = do
    args <- getArgs
    grockArg $ args !! 0
    where grockArg a
            | a == "u" = U.main U.userCommand
            | a == "a" = U.main $ U.aiCommand 3
            | a == "r" = R.main
            | otherwise = U.main U.userCommand
