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

updateSquare :: Grid -> Int -> Square
updateSquare grid i = case squares grid ! i of 
    Empty -> Empty
    Seat False -> if numOccupiedAdjacent == 0 then Seat True else Seat False
    Seat True -> if numOccupiedAdjacent >= 4 then Seat False else Seat True
    where
        numOccupiedAdjacent = countOccupiedSeats $ adjacent grid i

step :: Grid -> Grid
step grid = grid { squares = squares grid // changes }
    where 
        changes = zip indices $ map (updateSquare grid) indices
        indices = Array.indices $ squares grid

stabilize :: Grid -> Grid
stabilize grid 
    | next == grid = grid
    | otherwise    = stabilize next
    where
        next = step grid

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
    let final = stabilize grid
    putStrLn "End Grid"
    putStrLn "--------"
    print final
    putStrLn ""
    putStrLn $ "Ended with " ++ show (occupiedSeats final) ++ " seats occupied"
