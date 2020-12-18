module Main where

import Text.Parsec (string, (<|>), many1, digit, try, newline, endBy, choice, parse, eof, ParsecT, notFollowedBy)
import Data.Functor.Identity (Identity)
import Data.Char (isSpace, isDigit)

data Operator = Add | Mul deriving (Show)
data Expression = Constant Int | Calculation Expression Operator Expression deriving (Show)

apply :: Operator -> Int -> Int -> Int
apply Add = (+)
apply Mul = (*)

operator :: ParsecT String u Identity Operator
operator = do { string "+"; return Add } <|> do { string "*"; return Mul } 

number :: ParsecT String u Identity Int
number = read <$> many1 digit

parse3 :: Monad m => (t1 -> t2 -> t3 -> b) -> m t1 -> m t2 -> m t3 -> m b
parse3 f a b c = do
    a' <- a
    b' <- b
    f a' b' <$> c

constant :: ParsecT String u Identity Expression
constant = Constant <$> number

leftCalculation :: ParsecT String u Identity Expression
leftCalculation = parse3 Calculation constant operator expression

rightCalculation :: ParsecT String u Identity Expression
rightCalculation = parse3 Calculation bracketedExpression operator expression

bracketedExpression :: ParsecT String u Identity Expression
bracketedExpression = string "(" *> expression <* string ")"

expression :: ParsecT String u Identity Expression
expression = choice [ try (constant <* notFollowedBy operator)
                    , try leftCalculation
                    , try rightCalculation
                    , try bracketedExpression ]

resolve :: Expression -> Int
resolve (Constant x) = x
resolve (Calculation x op y) = resolve x `op'` resolve y
    where
        op' = apply op

tokenize [] = []
tokenize (c:cs) 
    | c == ' ' = tokenize cs
    | c == '(' = ")":tokenize cs
    | c == ')' = "(":tokenize cs
    | num /= "" =  num:tokenize rest
    | otherwise = [c]:tokenize cs
    where 
        (num, rest) = span isDigit (c:cs)


preprocess = concat . reverse . tokenize 

parseExpression :: String -> Expression
parseExpression text = case parse (expression <* eof) "" text' of
    Left e -> error $ "Failed to parse expression: " ++ show e
    Right es -> es
    where
        text' = preprocess text

main :: IO ()
main = do
    ls <- fmap lines getContents
    print $ sum $ map (resolve . parseExpression) ls
