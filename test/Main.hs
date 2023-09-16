module Main where

import Test.Hspec
import MagicSquareInternal

import MagicSquare

konesSpec :: Spec
konesSpec = describe "kones" $ do
    it "no ones" $ do
        kones [False, False] 0 `shouldBe` [[0, 0]]
    it "one ones" $ do
        kones [False, False, True] 1 `shouldBe` [[0, 0, 1]]
    it "two ones" $ do
        kones [True, False, True] 2 `shouldBe` [[1, 0, 1]]
    it "two ones anywhere" $ do
        kones [True, True, True] 2 `shouldBe` [[1, 1, 0], [1, 0, 1], [0, 1, 1]]
    it "two ones no free" $ do
        kones [False, False, True] 2 `shouldBe` []

firstTwoEightFour :: [[[Int]]]
firstTwoEightFour = [[[0,0,0,0,1,1,1,1],[0,0,0,0,1,1,1,1],[0,0,0,0,1,1,1,1],
                      [0,0,0,0,1,1,1,1],[1,1,1,1,0,0,0,0],[1,1,1,1,0,0,0,0],
                      [1,1,1,1,0,0,0,0],[1,1,1,1,0,0,0,0]],[[0,0,0,0,1,1,1,1],
                      [0,0,0,0,1,1,1,1],[0,0,0,0,1,1,1,1],[0,0,0,1,0,1,1,1],
                      [1,1,1,0,1,0,0,0],[1,1,1,1,0,0,0,0],[1,1,1,1,0,0,0,0],
                      [1,1,1,1,0,0,0,0]]]

magicSquaresSpec :: Spec
magicSquaresSpec = describe "magicSquares" $ do
    it "magicSquares 8 4 2" $ do
        take 2 (magicSquares 8 4)  `shouldBe` firstTwoEightFour

eightFourExample :: [[Int]]
eightFourExample = [[0,0,0,0,1,1,1,1],[0,0,0,0,1,1,1,1],[0,0,1,1,1,0,0,1],
                    [0,1,1,0,0,0,1,1],[1,1,0,0,0,1,1,0],[1,0,0,1,1,1,0,0],
                    [1,1,1,1,0,0,0,0],[1,1,1,1,0,0,0,0]]

solvedMagicSpec :: Spec
solvedMagicSpec = describe "solvedMagic" $ do
    it "solvedMagic 4 eightFourExample" $ do
        solvedMagic 4 (eightFourExample, [4,4,4,4,4,4,4,4]) `shouldBe` True

solvedConnectedSpec :: Spec
solvedConnectedSpec = describe "solvedConnected" $ do
    it "solvedConnected 4 eightFourExample" $ do
        solvedConnected 4 (eightFourExample, [4,4,4,4,4,4,4,4]) `shouldBe` True

solutionsMagicSpec :: Spec
solutionsMagicSpec = describe "solutions with solvedMagic" $ do
    it "solutions solvedMagic 1 ([], [0, 0])" $ do
        solutions solvedMagic 1 ([], [0, 0]) `shouldBe` [([[0, 1], [1, 0]], [1, 1]),
                                                         ([[1, 0], [0, 1]], [1, 1])]

main :: IO ()
main = hspec $ do
    konesSpec
    magicSquaresSpec
    solvedMagicSpec
    solvedConnectedSpec
    solutionsMagicSpec
