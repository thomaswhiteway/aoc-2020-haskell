module Main where

import Text.Parsec (parse, eof, endBy, newline, many1, digit, sepBy, char, lower, string, optional, (<|>))
import qualified Data.Set as Set

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

main :: IO ()
main = interact (show . Set.size . canContain "shiny gold" . parseRules)

