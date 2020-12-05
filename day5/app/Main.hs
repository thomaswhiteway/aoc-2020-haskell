module Main where

import Data.List (sort);

type BoardingPass = (Int, Int)

parseSection :: Char -> Int -> Int -> String -> Int
parseSection c = parseSection'
    where
        parseSection' :: Int -> Int -> String -> Int
        parseSection' lower _ [] = lower
        parseSection' lower upper (h:rest) 
            | h == c    = parseSection' lower  middle rest
            | otherwise = parseSection' middle upper  rest 
            where
                middle = (lower + upper) `div` 2

parseBoardingPass :: String -> BoardingPass
parseBoardingPass text = (parseSection 'F' 0 128 rowtext, 
                          parseSection 'L' 0 8 columntext)
    where
        (rowtext, columntext) = splitAt 7 text

getId :: BoardingPass -> Int
getId (row, column) = row * 8 + column

findMissing :: [Int] -> Int
findMissing (x:y:rest) 
    | y == x+1  = findMissing (y:rest)
    | otherwise = x+1

main :: IO ()
main = interact (show . findMissing . sort . map (getId . parseBoardingPass) . lines)
