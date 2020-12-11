module Main where

import qualified Data.Array.IArray as Array
import Data.Array ( (//), (!) )

data Grid = Grid { width :: Int,
                   height :: Int,
                   squares :: Array.Array Int Square } deriving (Eq)

data Square = Empty | Seat Bool deriving (Eq)

instance Show Square where
    show Empty        = "."
    show (Seat False) = "L"
    show (Seat True)  = "#"

instance Show Grid where
    show Grid{width=w,height=h,squares=sqrs} = unlines $ [concat [ show $ sqrs ! (row*w+col) | col <- [0..w-1]] | row <- [0..h-1]]

lookupLinear :: Grid -> Int -> Square
lookupLinear grid i = squares grid ! i

lookupGrid :: Grid -> (Int, Int) -> Maybe Square
lookupGrid grid pos 
    | within grid pos = Just $ lookupLinear grid $ gridToLinear grid pos
    | otherwise       = Nothing

within :: Grid -> (Int, Int) -> Bool
within Grid{height=h, width=w} (x, y) = x >= 0 && x < w && y >= 0 && y < h

linearToGrid :: Grid -> Int -> (Int, Int)
linearToGrid Grid{width=w} i = (i `mod` w, i `div` w)
 
gridToLinear :: Grid -> (Int, Int) -> Int
gridToLinear Grid{width=w} (x, y) = x + y * w

adjacentOffsets :: [(Int, Int)]
adjacentOffsets = [ (col, row) | col <- [-1, 0, 1], row <- [-1, 0, 1], row /= 0 || col /= 0 ]

adjacentIndices :: Grid -> Int -> [Int]
adjacentIndices grid i = [ gridToLinear grid (x', y') | (x', y') <- adjacentCoords, within grid (x', y') ]
    where
        (x, y) = linearToGrid grid i
        adjacentCoords = [ (x + x', y + y') | (x', y') <- adjacentOffsets ]

adjacent :: Grid -> Int -> [Square]
adjacent grid i = map (squares grid!) $ adjacentIndices grid i

findVisible :: Grid -> (Int, Int) -> (Int, Int) -> Square
findVisible grid (x, y) offset@(dx, dy) = case nextSquare of 
    Nothing    -> Empty
    Just Empty -> findVisible grid next offset
    Just seat  -> seat
    where
        next = (x + dx, y + dy)
        nextSquare = lookupGrid grid next 

visible :: Grid -> Int -> [Square]
visible grid i = map (findVisible grid $ linearToGrid grid i) adjacentOffsets

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

parseSquare :: Char -> Square
parseSquare '.' = Empty
parseSquare 'L' = Seat False
parseSquare '#' = Seat True

parse :: String -> Grid
parse text = Grid { width = w, height = h, squares = sqrs }
    where
        grid = map (map parseSquare) $ lines text
        h = length grid
        w = length $ head grid
        sqrs = Array.array (0, h*w-1) $ zip [0..] $ concat grid

updateSquareAdjacent :: Grid -> Int -> Square
updateSquareAdjacent grid i = case lookupLinear grid i of 
    Empty -> Empty
    Seat False -> if numOccupiedAdjacent == 0 then Seat True else Seat False
    Seat True -> if numOccupiedAdjacent >= 4 then Seat False else Seat True
    where
        numOccupiedAdjacent = countOccupiedSeats $ adjacent grid i

updateSquareVisible :: Grid -> Int -> Square
updateSquareVisible grid i = case lookupLinear grid i of 
    Empty -> Empty
    Seat False -> if numOccupiedVisible == 0 then Seat True else Seat False
    Seat True -> if numOccupiedVisible >= 5 then Seat False else Seat True
    where
        numOccupiedVisible = countOccupiedSeats $ visible grid i

step :: (Grid -> Int -> Square) ->  Grid -> Grid
step update grid = grid { squares = squares grid // changes }
    where 
        changes = zip indices $ map (update grid) indices
        indices = Array.indices $ squares grid

stabilize :: (Grid -> Int -> Square) -> Grid -> Grid
stabilize update grid 
    | next == grid = grid
    | otherwise    = stabilize update next
    where
        next = step update grid

countOccupiedSeats :: [Square] -> Int
countOccupiedSeats = count (==Seat True)

occupiedSeats :: Grid -> Int
occupiedSeats = countOccupiedSeats . Array.elems . squares

main :: IO ()
main = do
    grid <- fmap parse getContents
    putStrLn "Start Grid"
    putStrLn "----------"
    print grid
    putStrLn ""
    let finalAdjacent = stabilize updateSquareAdjacent grid
    putStrLn "End Grid (Adjacent)"
    putStrLn "-------------------"
    print finalAdjacent
    putStrLn ""
    let finalVisible = stabilize updateSquareVisible grid
    putStrLn "End Grid (Visible)"
    putStrLn "-------------------"
    print finalVisible
    putStrLn ""
    putStrLn $ "Adjacent: Ended with " ++ show (occupiedSeats finalAdjacent) ++ " seats occupied"
    putStrLn $ "Visible: Ended with " ++ show (occupiedSeats finalVisible) ++ " seats occupied"
