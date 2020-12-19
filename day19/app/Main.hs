module Main where

import Text.Parsec ( string, letter, char, many1, digit, sepBy, (<|>), ParsecT, try, newline, endBy, parse, choice, sepEndBy, eof )
import Control.Monad ( mapM, mapM_ )
import Data.IntMap.Lazy ( IntMap, (!) )
import qualified Data.IntMap.Lazy as IntMap
import Data.Functor.Identity (Identity)
import Data.Either (isRight)
import Data.Maybe (fromJust, mapMaybe)
import Data.List (intercalate)

data Rule = Literal Char | Ref Int | Sequence [Rule] | Either [Rule] deriving (Show)
data ParseTree = Lit Char | Seq [ParseTree] | Eit [ParseTree] 

multiCombinator :: ParseTree -> Bool
multiCombinator (Lit _) = False
multiCombinator (Eit ts) = any hasSeq ts
multiCombinator (Seq ts) = any hasEither ts

hasEither :: ParseTree -> Bool
hasEither (Lit _) = False
hasEither (Seq ts) = any hasEither ts
hasEither (Eit _) = True

hasSeq :: ParseTree -> Bool
hasSeq (Lit _) = False
hasSeq (Eit ts) = any hasSeq ts
hasSeq (Seq _) = True

instance Show ParseTree where
    show (Lit c)  = [c]
    show (Seq ts) = concatMap (\t -> if multiCombinator t then  "(" ++ show t ++ ")" else show t) ts
    show (Eit ts) = intercalate "|" $ map (\t -> if multiCombinator t then  "(" ++ show t ++ ")" else show t) ts

literal = Literal <$> (char '"' *> letter <* char '"')
reference = Ref . read <$> many1 digit
sequence = Sequence <$> ((literal <|> reference) `sepEndBy`  char ' ')
either = Either <$> (Main.sequence `sepBy` string "| " )
ruleNumber = read <$> many1 digit
rule = do
    num <- ruleNumber
    string ": "
    r <- Main.either
    return (num, r)

rules = rule `endBy` newline

message = many1 letter
messages = message `endBy` newline

input = do
    rs <- rules
    newline
    ms <- messages
    return (rs, ms)

parseInput :: String -> ([(Int, Rule)], [String])
parseInput text = case parse input "" text of 
    Left e -> error $ "Failed to parse input: " ++ show e
    Right i -> i

compile :: ParseTree -> ParsecT String u Identity String 
compile (Seq children) = concat <$> mapM compile children
compile (Eit children) = choice $ map (try . compile) children
compile (Lit c)        = (:[]) <$> char c

parseTree :: IntMap Rule -> Rule -> ParseTree
parseTree rs (Ref index)         = parseTree rs (rs ! index)
parseTree rs (Sequence children) = Seq $ map (parseTree rs) children
parseTree rs (Either children)   = Eit $ map (parseTree rs) children
parseTree rs (Literal c)         = Lit c

normalize :: ParseTree -> Maybe ParseTree
normalize (Lit c)  = Just $ Lit c
normalize (Seq xs) = case mapMaybe normalize xs of 
    [] -> Nothing
    [x] -> Just x
    xs' -> Just $ Seq xs'
normalize (Eit xs) = case mapMaybe normalize xs of 
    [] -> Nothing
    [x] -> Just x
    xs' -> Just $ Eit xs'

isValid :: ParsecT String () Identity String -> String -> Bool
isValid p text = isRight $ parse (p <* eof) "" text

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f 

main :: IO ()
main = do
    (rs, messages) <- fmap parseInput getContents
    let indirectRules = IntMap.fromList rs
    let pt = fromJust $ normalize $ parseTree indirectRules (indirectRules ! 0)
    let p = compile pt
    mapM_ (\m -> putStrLn $ m ++ " " ++ show (isValid p m)) messages
    putStrLn $ show (count (isValid p) messages) ++ " messages are valid"

    
