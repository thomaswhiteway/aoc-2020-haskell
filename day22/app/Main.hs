module Main where

import Text.Parsec (parse, digit, many1, string, newline, endBy)
import Data.Char (digitToInt)

player = string "Player " *> (digitToInt <$> digit) <* string ":" <* newline
card = read <$> many1 digit
deck = player *> card `endBy` newline

game = do
    p1 <- deck
    newline
    p2 <- deck
    return (p1, p2)

parseGame :: String -> ([Int], [Int])
parseGame text = case parse game "" text of
    Left e -> error $ "Failed to parse game: " ++ show e 
    Right g -> g

winningHand :: Ord a => [a] -> [a] -> [a]
winningHand []      d2      = d2
winningHand d1      []      = d1
winningHand (c1:d1) (c2:d2) 
    | c1 > c2   = winningHand (d1 ++ [c1, c2]) d2
    | otherwise = winningHand d1               (d2 ++ [c2, c1])

winningScore :: [Int] -> [Int] -> Int
winningScore d1 d2 = score $ winningHand d1 d2

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

main :: IO ()
main = do
    (d1, d2) <- parseGame <$> getContents 
    print $ winningScore d1 d2
