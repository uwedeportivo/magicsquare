module MagicSquare  (magicSquares, connectedSquares) where

import MagicSquareInternal
    ( solutions, solvedConnected, solvedMagic )

magicSquares :: Int -> Int -> [[[Int]]]
magicSquares size numOnes = map fst (solutions solvedMagic numOnes ([], [0::Int | _ <- [1..size]]))

connectedSquares :: Int -> Int -> [[[Int]]]
connectedSquares size numOnes = map fst (solutions solvedConnected numOnes ([], [0::Int | _ <- [1..size]]))
