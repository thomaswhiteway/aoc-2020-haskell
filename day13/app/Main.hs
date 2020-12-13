{-# LANGUAGE TupleSections #-}
module Main where

import Text.Parsec (parse, digit, many1, string, (<|>), sepBy)
import Data.Maybe (mapMaybe, fromJust)
import Data.List (find)

bus = fmap (Just . read) (many1 digit) 
    <|>  do { string "x"; return Nothing }

buses = bus `sepBy` string ","

parseBuses :: String -> [(Int, Int)]
parseBuses text = case parse buses "" text of
    Left e -> error $ "Failed to parse buses " ++ show e
    Right bs -> mapMaybe (\(i, t) -> fmap (i,) t) $ zip [0..] bs

timeToBus :: Int -> Int -> Int
timeToBus start interval = (interval - (start `mod` interval)) `mod` interval

timeToNextBus :: Int -> [(Int, Int)] -> (Int, Int)
timeToNextBus start buses = minimum $ zip (map (timeToBus start) buses') buses'
    where
        buses' = map snd buses

firstConsecutiveDepartures :: [(Int, Int)] -> Int
firstConsecutiveDepartures = fst . foldr firstDepartureWithOffset (0, 1) 

firstDepartureWithOffset :: (Int, Int) -> (Int, Int) -> (Int, Int)
firstDepartureWithOffset (index, bus) (earliest, multiplier) = (t, lcm multiplier bus)
    where
        t = fromJust $ 
            find (\t -> (t + index) `mod` bus == 0) $ 
            map (\q -> earliest + multiplier * q) [0..]

main :: IO ()
main = do
    arrivalTime <- fmap read getLine
    buses <- fmap parseBuses getLine
    let (waitTime, firstBus) = timeToNextBus arrivalTime buses
    putStrLn $ "Next bus (" ++ show firstBus ++ ") leaves in " ++ show waitTime ++ " minutes"
    putStrLn $ "next bus * wait time = " ++ show (firstBus * waitTime)
    let consecutiveTime = firstConsecutiveDepartures buses
    putStrLn $ "First set of consecutive departures is at " ++ show consecutiveTime