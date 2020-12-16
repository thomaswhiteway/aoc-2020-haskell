module Main where

import Text.Parsec ( ParsecT, parse, char, digit, many1, sepBy, string, newline, endBy, letter, eof, (<|>) )
import Data.Functor.Identity ( Identity )

data Field = Field String [(Int, Int)] deriving (Eq, Show)
type Ticket = [Int]

ticket :: ParsecT String u Identity [Int]
ticket = fmap read (many1 digit) `sepBy` char ','

range :: ParsecT String u Identity (Int, Int)
range = do
    lower <- many1 digit
    char '-'
    upper <- many1 digit
    return (read lower, read upper)

field :: ParsecT String u Identity Field
field = do
    name <- many1 (letter <|> char ' ')
    string ": "
    ranges <- range `sepBy` string " or "
    return $ Field name ranges

fields :: ParsecT String u Identity [Field]
fields = field `endBy` newline

yourTicket :: ParsecT String u Identity [Int]
yourTicket = do
    string "your ticket:" <* newline
    ticket <* newline

nearbyTickets :: ParsecT String u Identity [[Int]]
nearbyTickets = do
    string "nearby tickets:" <* newline
    ticket `endBy` newline

input :: ParsecT String u Identity ([Field], [Int], [[Int]])
input = do
    fs <- fields
    newline
    t <- yourTicket
    newline
    ts <- nearbyTickets
    return (fs, t, ts)

parseInput :: String -> ([Field], Ticket, [Ticket])
parseInput text = case parse (input <* eof) "" text of
    Left e -> error $ "Failed to parse input: " ++ show e
    Right i -> i

inRange :: Int -> (Int, Int) -> Bool
inRange v (low, high) = low <= v && v <= high

valueValid :: Int -> Field -> Bool
valueValid v (Field _ rs) = any (inRange v) rs

validFields :: Int -> [Field] -> [Field]
validFields v = filter $ valueValid v

canBeValid :: [Field] -> Int -> Bool
canBeValid fs v = validFields v fs /= []

invalidValues :: [Field] -> Ticket -> [Int]
invalidValues fs = filter $ not . canBeValid fs

main :: IO ()
main = do
    (fs, t, ts) <- fmap parseInput getContents 
    let vs = concatMap (invalidValues fs) ts
    putStrLn $ "Ticket scanning error rate is " ++ show (sum vs)
