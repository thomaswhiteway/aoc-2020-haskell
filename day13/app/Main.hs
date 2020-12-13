module Main where

import Text.Parsec (parse, digit, many1, string, (<|>), sepBy)
import Data.Maybe (catMaybes)

bus = fmap (Just . read) (many1 digit) 
    <|>  do { string "x"; return Nothing }

buses = bus `sepBy` string ","

parseBuses :: String -> [Maybe Int]
parseBuses text = case parse buses "" text of
    Left e -> error $ "Failed to parse buses " ++ show e
    Right bs -> bs

timeToBus :: Int -> Int -> Int
timeToBus start interval = (interval - (start `mod` interval)) `mod` interval

timeToNextBus :: Int -> [Maybe Int] -> (Int, Int)
timeToNextBus start buses = minimum $ zip (map (timeToBus start) buses') buses'
    where
        buses' = catMaybes buses

main :: IO ()
main = do
    arrivalTime <- fmap read getLine
    buses <- fmap parseBuses getLine
    let (waitTime, firstBus) = timeToNextBus arrivalTime buses
    putStrLn $ "Next bus (" ++ show firstBus ++ ") leaves in " ++ show waitTime ++ " minutes"
    putStrLn $ "next bus * wait time = " ++ show (firstBus * waitTime)