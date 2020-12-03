module Main where

istree :: Char -> Bool
istree '#' = True
istree _ = False

solve :: [String] -> Int
solve slope = length $ filter istree [Main.lookup slope (3 * y)  y | y <- [0..length slope-1]]

lookup :: [String] -> Int -> Int -> Char
lookup slope x y = row !! (x `mod` length row)
    where
        row = slope !! y

main :: IO ()
main = interact (show . solve . lines)
