module Main where

import Text.Parsec ( ParsecT, parse, char, digit, many1, sepBy1, string, newline, endBy, letter, eof, (<|>) )
import Data.Functor.Identity ( Identity )
import Data.List ( transpose, isPrefixOf, sort, sortOn )
import Control.Monad (msum)
import Data.Bifunctor ( Bifunctor(second) )

data Field = Field String [(Int, Int)] deriving (Eq, Show)
type Ticket = [Int]

fieldName :: Field -> String
fieldName (Field name _) = name

ticket :: ParsecT String u Identity [Int]
ticket = fmap read (many1 digit) `sepBy1` char ','

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
    ranges <- range `sepBy1` string " or "
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

fieldsValidForValue :: Int -> [Field] -> [Field]
fieldsValidForValue v = filter $ valueValid v

fieldsValidForValues :: [Int] -> [Field] -> [Field]
fieldsValidForValues []     fs = fs
fieldsValidForValues _      [] = []
fieldsValidForValues (v:vs) fs = fieldsValidForValues vs fs'
    where 
        fs' = fieldsValidForValue v fs

canBeValid :: [Field] -> Int -> Bool
canBeValid fs v = fieldsValidForValue v fs /= []

ticketCanBeValid :: [Field] -> Ticket -> Bool
ticketCanBeValid fs t = null $ invalidValues fs t

invalidValues :: [Field] -> Ticket -> [Int]
invalidValues fs = filter $ not . canBeValid fs

without :: [(Int, [String])] -> String -> [(Int, [String])]
without fs name = map (second $ filter (/=name)) fs

findFieldOrder :: [[String]] -> Maybe [String]
findFieldOrder fs =  map snd . sort <$> findFieldOrderWithIndex sortedFieldsWithIndex
    where       
        sortedFieldsWithIndex :: [(Int, [String])]
        sortedFieldsWithIndex = sortOn (length . snd) $ zip [0..] fs

findFieldOrderWithIndex :: [(Int, [String])] -> Maybe [(Int, String)]
findFieldOrderWithIndex []        = Just []
findFieldOrderWithIndex ((i, fs):rest) = msum [     
        ((i,f):) <$> findFieldOrderWithIndex (rest `without` f) | f <- fs
    ]

main :: IO ()
main = do
    (fs, t, ts) <- fmap parseInput getContents 
    let vs = concatMap (invalidValues fs) ts
    putStrLn $ "Ticket scanning error rate is " ++ show (sum vs)
    let validTickets = filter (ticketCanBeValid fs) ts
    let fieldValues = transpose validTickets
    let fieldsForPosition = [map fieldName $ fieldsValidForValues vs fs | vs <- fieldValues]
    case findFieldOrder fieldsForPosition of
        Nothing -> putStrLn "No Solution"
        Just fieldOrder -> do
            mapM_ putStrLn fieldOrder
            let result = product [ val | (name, val) <- zip fieldOrder t, "departure" `isPrefixOf` name]
            putStrLn $ "Result is " ++ show result




