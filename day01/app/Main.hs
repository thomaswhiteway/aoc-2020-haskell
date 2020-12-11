module Main where

import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.List.Extra(firstJust)

solve :: Int -> [Int] -> Int
solve target values = getSolution $ fromJust $ firstJust canSum values
    where 
        entrySet = Map.fromList [(a + b, (a, b)) | a <- values, b <- values]
        canSum v = fmap (\(a, b) -> (a, b, v)) $ Map.lookup (target-v) entrySet
        getSolution (a, b, c) = a * b * c

parse :: String -> [Int]
parse = map read . lines

main :: IO ()
main = interact (show . solve 2020 . parse)
