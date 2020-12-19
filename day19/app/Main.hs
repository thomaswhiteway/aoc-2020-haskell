module Main where

import Text.Parsec ( string, letter, char, many1, digit, sepBy, (<|>), ParsecT, try, newline, endBy, parse, choice, sepEndBy, eof )
import Control.Monad ( mapM, mapM_ )
import Data.IntMap.Lazy ( IntMap, (!) )
import qualified Data.IntMap.Lazy as IntMap
import Data.Functor.Identity (Identity)
import Data.Either (isRight)
import Data.Maybe (fromJust, mapMaybe, isJust)
import Data.List (intercalate, find)
import Data.Bifunctor (second)

data Rule = Literal Char | Ref Int | Sequence [Rule] | Either [Rule] deriving (Eq)
data ParseResult = Lit Char | Rf Int ParseResult | Seq [ParseResult] 

multiCombinator :: Rule -> Bool
multiCombinator (Literal _) = False
multiCombinator (Ref _) = False
multiCombinator (Either ts) = any hasSequence ts
multiCombinator (Sequence ts) = any hasEither ts

hasEither :: Rule -> Bool
hasEither (Literal _) = False
hasEither (Sequence ts) = any hasEither ts
hasEither (Either _) = True
hasEither (Ref _) = False

hasSequence :: Rule -> Bool
hasSequence (Literal _) = False
hasSequence (Either ts) = any hasSequence ts
hasSequence (Sequence _) = True
hasSequence (Ref _) = False

instance Show Rule where
    show (Literal c)  = [c]
    show (Sequence ts) = concatMap (\t -> if multiCombinator t then  "(" ++ show t ++ ")" else show t) ts
    show (Either ts) = intercalate "|" $ map (\t -> if multiCombinator t then  "(" ++ show t ++ ")" else show t) ts
    show (Ref i) = "(" ++ show i ++ ")"

instance Show ParseResult where
    show (Lit c)  = [c]
    show (Seq ts) = concatMap show ts
    show (Rf i r) = show i ++ ":(" ++ show r ++ ")"

literal = Literal <$> (char '"' *> letter <* char '"')
reference = Ref . read <$> many1 digit
sequence = Sequence <$> ((literal <|> reference) `sepEndBy`  char ' ')
either = Either <$> (Main.sequence `sepBy` string "| " )
ruleNumber = read <$> many1 digit
rule = do
    num <- ruleNumber
    string ": "
    r <- fromJust . normalize <$> Main.either
    return (num, r)

rules = rule `endBy` newline

message = many1 letter
messages = message `endBy` newline

input = do
    rs <- rules
    newline
    ms <- messages
    return (rs, ms)

parseRule :: String -> (Int, Rule)
parseRule text = case parse (rule <* eof) "" text of 
    Left e -> error $ "Failed to parse rule: " ++ show e
    Right r -> r

parseInput :: String -> ([(Int, Rule)], [String])
parseInput text = case parse input "" text of 
    Left e -> error $ "Failed to parse input: " ++ show e
    Right i -> i

normalize :: Rule -> Maybe Rule
normalize (Literal c)  = Just $ Literal c
normalize (Ref i) = Just $ Ref i
normalize (Sequence xs) = case mapMaybe normalize xs of 
    [] -> Nothing
    [x] -> Just x
    xs' -> Just $ Sequence xs'
normalize (Either xs) = case mapMaybe normalize xs of 
    [] -> Nothing
    [x] -> Just x
    xs' -> Just $ Either xs'

tryParse :: IntMap Rule -> Rule -> String -> [(ParseResult, String)]
tryParse rules (Literal _)       []     = []
tryParse rules (Literal c)       (x:xs) = [(Lit c, xs) | c == x]
tryParse rules (Ref i)           text   = [(Rf i r, rest) | (r, rest) <- tryParse rules (rules ! i) text]
tryParse rules (Sequence [])     text   = [(Seq [], text)]
tryParse rules (Sequence (x:xs)) text   = [(Seq (r:rs), rest') | (r, rest) <- tryParse rules x text, (Seq rs, rest') <- tryParse rules (Sequence xs) rest]
tryParse rules (Either xs)       text   = [rest | x <- xs, rest <- tryParse rules x text]

isValid :: IntMap Rule -> Rule -> String -> Maybe ParseResult
isValid rules rule text = fmap fst $ find (null . snd) $ tryParse rules rule text

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f 

main :: IO ()
main = do
    (rs, messages) <- fmap parseInput getContents
    let rules = IntMap.fromList rs
    let r = rules ! 0
    mapM_ (\m -> putStrLn $ m ++ " " ++ show (isValid rules r m)) messages
    putStrLn $ show (count (isJust . isValid rules r) messages) ++ " messages are valid"
    let rules' = IntMap.fromList [ parseRule "8: 42 | 42 8"
                                        , parseRule "11: 42 31 | 42 11 31"
                                        ] `IntMap.union` rules
    mapM_ (\m -> putStrLn $ m ++ " " ++ show (isValid rules' r m)) messages
    putStrLn $ show (count (isJust . isValid rules' r) messages) ++ " messages are valid"
    
