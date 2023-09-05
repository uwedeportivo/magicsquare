module Main where

import MagicSquare
import System.Environment (getArgs)

puzzle :: [String] -> IO ()

puzzle args = 
    case args of
        [sizeStr, numOnesStr] -> putStrLn $ show ms
               where
                 ms = take 1 (connectedSquares (read sizeStr) (read numOnesStr))
        _ -> putStrLn "missing size and number of ones"


main :: IO ()
main = do
    args <- getArgs
    puzzle args
