module Main where

import System.Environment (getArgs)
import Data.Char (digitToInt)
import Util (nTimes)

findNext :: Int -> [Int] -> Int
findNext (-1) xs = maximum xs
findNext    n xs 
    | n `elem` xs = n
    | otherwise   = findNext (n-1) xs

insertAfter :: Int -> [Int] -> [Int] -> [Int]
insertAfter n toInsert (x:xs)
    | n == x    = (x : toInsert) ++ xs
    | otherwise = x : insertAfter n toInsert xs

step :: [Int] -> [Int]
step (x:xs) = xs' ++ [x]
    where
        (removed, rest) = splitAt 3 xs
        xs' = insertAfter next removed rest
        next = findNext (x-1) rest

rotateTo :: Int -> [Int] -> [Int]
rotateTo n (x:xs) 
    | n == x    = x:xs
    | otherwise = rotateTo n (xs ++ [x])

main :: IO ()
main = do
    sequence <- map digitToInt . head <$> getArgs
    let sequence' = nTimes 100 step sequence
    putStrLn $ concatMap show $ tail $ rotateTo 1 sequence'
