module Main where

import Text.Parsec (parse, eof, endBy, newline, many1, digit, sepBy, char, lower, string, optional, (<|>))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import Data.Either (partitionEithers)

type Bag = String;

type Rule = (Bag, [(Int, Bag)])

contains :: Bag -> Rule -> Bool
contains b (_, contents) = any (\(_, c) -> c == b) contents

bag = (many1 lower) <> (string  " ") <> (many1 lower) 

emptyRule = do { string "no other bags"; return [] }
containsClause = do
    n <- fmap read $ many1 digit
    char ' '
    b <- bag
    if n > 1 then string " bags" else string " bag"
    return (n, b)

rule = do
    b <- bag
    string " bags contain "
    cs <- emptyRule <|> (containsClause `sepBy` string ", ")
    char '.'
    return (b, cs)

rules = rule `endBy` newline

parseRules :: String -> [Rule]
parseRules text = case parse (rules <* eof) "" text of
    Left e -> error ("Failed to parse rules " ++ show e)
    Right rs -> rs

canContain :: Bag -> [Rule] -> Set.Set Bag
canContain b rs = Set.unions (Set.fromList containers:[canContain c rs | c <- containers])
    where
        containers = map fst $ filter (contains b) rs

foldrMaybes :: (a -> b -> b) -> b -> [Maybe a] -> Maybe b
foldrMaybes f init = foldr (liftM2 f) (Just init)

numContains :: Bag -> [Rule] -> Int
numContains b rs = fromJust $ Map.lookup b $ allContains rs

allContains :: [Rule] -> Map.Map Bag Int
allContains rs = allContains' rs Map.empty

allContains' :: [Rule] -> Map.Map Bag Int -> Map.Map Bag Int
allContains' [] m = m
allContains' rs' m = allContains' remaining (m `Map.union` Map.fromList new)
    where
        (new, remaining) = partitionEithers [ case numContains' cs m of 
            Nothing -> Right (c, cs)
            Just n -> Left (c, n) | (c, cs) <- rs']

numContains' :: [(Int, Bag)] -> Map.Map Bag Int -> Maybe Int           
numContains' contents counts = foldrMaybes (+) 0
    [fmap (\contained -> required * (contained + 1)) $ Map.lookup bag counts 
        | (required, bag) <- contents]

main :: IO ()
main = interact (show . numContains "shiny gold" . parseRules)

