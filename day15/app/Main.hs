module Main where

import Data.IntMap ( IntMap, (!?) )
import qualified Data.IntMap as IntMap
import Text.Parsec (parse, sepBy, digit, many1, char)
import Data.Maybe (maybe)

prefix = fmap read (many1 digit) `sepBy` char ','

parsePrefix :: String -> [Int]
parsePrefix text = case parse prefix "" text of 
    Left e -> error $ "Failed to parse prefix: " ++ show e
    Right p -> p

next :: IntMap Int -> Int -> Int -> (IntMap Int, Int, Int)
next history index value = (history', index', value')
    where
        history' = IntMap.insert value index history
        index' = index + 1
        value' =  maybe 0 (index-) (history !? value)

speak :: [Int] -> [Int]
speak pref = pref ++ speak' history (length pref - 1) (last pref)
    where history = IntMap.fromList $ zip (init pref) [0..]


speak' :: IntMap Int -> Int -> Int -> [Int]
speak' history index value = value' : speak' history' index' value'
    where
        (history', index', value') = next history index value

main :: IO ()
main = do 
    pref <- fmap parsePrefix getLine
    let sequence = speak pref
    putStrLn $ "The 2020th number spoken is " ++ show (sequence !! 2019)
    putStrLn $ "The 30000000th number spoken is " ++ show (sequence !! (30000000 - 1))

