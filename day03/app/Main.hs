module Main where

istree :: Char -> Bool
istree '#' = True
istree _ = False

gradients :: [(Int, Int)]
gradients = [
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)]

solve :: [String] -> Int
solve slope = product $ map (numTrees slope) gradients

numTrees :: [String] -> (Int, Int) -> Int
numTrees slope (dx, dy) = length $ filter istree $ map (uncurry $ Main.lookup slope) $ takeWhile (\(_, y) -> y < length slope) [(dx * i, dy * i) | i <- [0..]]

lookup :: [String] -> Int -> Int -> Char
lookup slope x y = row !! (x `mod` length row)
    where
        row = slope !! y

main :: IO ()
main = interact (show . solve . lines)
