module Main where

import Text.Parsec (parse, digit, many1, string, newline, endBy)
import Data.Char (digitToInt)
import Data.Set (Set)
import qualified Data.Set as Set

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

winnerRecursive :: [Int] -> [Int] -> (Int, [Int])
winnerRecursive = winnerRecursive' Set.empty
    where 
        winnerRecursive' _ d  [] = (1, d)
        winnerRecursive' _ [] d' = (2, d')
        winnerRecursive' visited d@(c:cs) d'@(c':cs')
            | (d, d') `Set.member` visited        = (1, d)
            | roundWinner == 1                    = winnerRecursive' visited' (cs ++ [c, c']) cs'
            | otherwise                           = winnerRecursive' visited' cs               (cs' ++ [c', c])
            where
                visited' = Set.insert (d, d') visited
                roundWinner
                    | length cs >= c && length cs' >= c' = winningPlayerRecursive (take c cs) (take c' cs') 
                    | c > c'                             = 1
                    | otherwise                          = 2

winningHandRecursive :: [Int] -> [Int] -> [Int]
winningHandRecursive d d' = snd $ winnerRecursive d d'

winningPlayerRecursive :: [Int] -> [Int] -> Int
winningPlayerRecursive d d' = fst $ winnerRecursive d d'

winningScoreRecursive :: [Int] -> [Int] -> Int
winningScoreRecursive d1 d2 = score $ winningHandRecursive d1 d2

main :: IO ()
main = do
    (d1, d2) <- parseGame <$> getContents 
    print $ winningScore d1 d2
    print $ winningScoreRecursive d1 d2
