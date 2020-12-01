module Main where

import qualified Data.IntSet as IntSet
import Data.Maybe
import Data.List

solve :: Int -> [Int] -> Int
solve target values = getSolution $ fromJust $ find canSum values
    where 
        entrySet = IntSet.fromList values
        canSum v = IntSet.member (target-v) entrySet
        getSolution v = v * (target-v)

parse :: String -> [Int]
parse = map read . lines

main :: IO ()
main = interact (show . solve 2020 . parse)
