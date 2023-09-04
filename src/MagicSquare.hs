module MagicSquare  (magicSquares, connectedSquares) where

import Data.Array
import Data.Bits

allTrue :: (a -> Bool) -> [a] -> Bool
allTrue f xs = foldr (&&) True (map f xs)

rowColumnToNode :: Int -> (Int, Int) -> Int
rowColumnToNode rows (row, column) = row * rows + column

nodeToRowColumn :: Int -> Int -> (Int, Int)
nodeToRowColumn rows vertex = (div vertex rows, mod vertex rows)

listToArray :: [a] -> Array Int a
listToArray xs = array idxs (zip (range idxs) xs)
  where
    idxs = (0, (length xs - 1))

matrixToArray :: [[a]] -> Array Int (Array Int a)
matrixToArray m = listToArray rs
  where
   rs = map listToArray m

kones :: [Bool] -> Int-> [[Int]]
kones cs 0 = [[0 | _ <- [1..n]]]
   where
    n = length cs
kones [] _ = []
kones (c:cs) k = if c then 
                          (map (1:) (kones cs (k-1))) ++ (map (0:) (kones cs k)) 
                      else 
                          (map (0:) (kones cs k))

type Node = Int
type Adj = [Node]
type Context = (Node, Adj)
type Graph = [Context]

without :: Node -> Adj -> Adj
without _ [] = []
without y (x:xs) = if x == y then xs else x:(without y xs)

findNode :: Node -> Graph -> Maybe (Int, Context)
findNode _ [] = Nothing
findNode u (v:g) = if u == fst v then Just (0, v) else 
        case (findNode u g) of
          Just (n, v) -> Just ((n+1), v)
          Nothing -> Nothing

transferEdge :: Context -> (Context, Graph) -> (Context, Graph)
transferEdge (v, onV) ((matched, onMatched), xs) = if (elem matched onV) then 
         ((matched, v:onMatched), (v, without matched onV):xs)
    else
         ((matched, onMatched), (v, onV): xs)

match :: Node -> Graph -> Maybe (Context, Graph)
match x xs = case idx of
    Just (n, c) -> 
        let
          prefix = take n xs
          (c', prefix') = foldr transferEdge (c, []) prefix
          suffix = drop (n+1) xs
        in
          Just (c', prefix' ++ suffix)
    Nothing -> Nothing
  where
    idx = findNode x xs

dfs :: [Node] -> Graph -> [Node]
dfs [] _ = []
dfs (v:vs) g = case m of
    Just (c, g') -> (fst c):(dfs ((snd c) ++ vs) g')
    Nothing -> dfs vs g
  where
    m = match v g

type MatrixArray = Array Int (Array Int Int)

matrixToGraphHelper' :: Int -> MatrixArray -> (Int, Int) -> Graph -> Graph
matrixToGraphHelper' size ma (row, column) g = ctx:g 
      where
        node = rowColumnToNode size (row, column)
        orthoNorthNeighbor :: (Int, Int) -> [Int] -> [Int]
        orthoNorthNeighbor (r, c) ns = if row > 0 then 
              if (ma ! (r - 1) ! c)  == 1 then 
                  (rowColumnToNode size (r -1, c)):ns 
              else ns
          else ns
        orthoWestNeighbor :: (Int, Int) -> [Int] -> [Int]
        orthoWestNeighbor  (r, c) ns = if c > 0 then 
                      if (ma ! r ! (c - 1))  == 1 then 
                         rowColumnToNode size (r, (c-1)):ns 
                      else ns
                  else ns
        adj = orthoNorthNeighbor (row, column) (orthoWestNeighbor (row, column) [])
        ctx = (node, adj)

matrixToGraphHelper :: Int -> MatrixArray -> (Int, Int) -> Graph -> Graph
matrixToGraphHelper size ma (row, column) g = 
     if (ma ! row ! column) == 1 then matrixToGraphHelper' size ma (row, column) g else g

matrixToGraph :: Int -> MatrixArray -> Graph
matrixToGraph size ma = foldr f [] idxs
  where
    idxs = [(size - i - 1, size - j - 1) | i <- [0..(size-1)], j <- [0..(size-1)]]
    f = matrixToGraphHelper size ma

type State = ([[Int]], [Int])
type Move = [Int]

moves :: Int -> State -> [Move]
moves k s = if ((length rows) == (length csum)) then [] else kones cs k
  where
   (rows, csum) = s
   cs = map ((>) k) csum

move :: State -> Move -> State
move s m = (m:rows, map (\x -> (fst x) + (snd x)) (zip m cs))
  where
    (rows, cs) = s

succs :: Int -> State -> [State]
succs k t = [move t m | m <- (moves k t)]

solvedMagic :: Int -> State -> Bool
solvedMagic k s = (length rows) == size && (allTrue (k ==) cs)
  where 
    (rows, cs) = s
    size = length cs
    
solvedConnected :: Int -> State -> Bool
solvedConnected k s = (length rows) == size && (allTrue (k ==) cs) && (length vs == length g)
  where 
    (rows, cs) = s
    size = length cs
    ma = matrixToArray rows
    g = matrixToGraph size ma
    sn = fst (head g)
    vs = dfs [sn] g

search :: (Int -> State -> Bool) -> Int -> [State] -> [State]
search _ _ [] = []
search solved k (t:ts) = if (solved k t) then t:(search solved k ts) else search solved k (succs k t ++ ts)

solutions :: (Int -> State -> Bool) -> Int -> State -> [State]
solutions solved k t = search solved k [t]

magicSquares :: Int -> Int -> [[[Int]]]
magicSquares size numOnes = map fst (solutions solvedMagic numOnes ([], [0::Int | _ <- [1..size]]))

connectedSquares :: Int -> Int -> [[[Int]]]
connectedSquares size numOnes = map fst (solutions solvedConnected numOnes ([], [0::Int | _ <- [1..size]]))
