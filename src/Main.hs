module Main where

import MagicSquare
import System.Environment (getArgs)

puzzle :: [String] -> IO ()

puzzle args = 
    case args of
        [sizeStr, numOnesStr, numTakes] -> putStrLn $ show ms
               where
                 ms = take (read numTakes) (connectedSquares (read sizeStr) (read numOnesStr))
        _ -> putStrLn "missing size, umber of ones and number of takes"


main :: IO ()
main = do
    args <- getArgs
    puzzle args
