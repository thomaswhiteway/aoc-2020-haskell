module Main where

import Data.Functor.Identity (Identity)
import Data.Char (isSpace, isDigit)
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe, fromJust)

data Op = Add | Mul deriving (Show, Eq)
data Expression = Constant Int | Calculation Expression Op Expression deriving (Show)

data Token = LeftBracket | RightBracket | TokAdd | TokMul  | Num Int deriving (Eq, Show)

data Tree v a = Node v [Tree v a] | Leaf a deriving (Eq, Show)

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Mul = (*)

resolve :: Expression -> Int
resolve (Constant x) = x
resolve (Calculation x op y) = resolve x `op'` resolve y
    where
        op' = apply op

tokenize [] = []
tokenize (c:cs) 
    | c == ' ' = tokenize cs
    | c == '(' = LeftBracket:tokenize cs
    | c == ')' = RightBracket:tokenize cs
    | c == '+' = TokAdd:tokenize cs
    | c == '*' = TokMul:tokenize cs
    | num /= "" =  Num (read num):tokenize rest
    where 
        (num, rest) = span isDigit (c:cs)

parseBracket :: [Token] -> ([Tree (Maybe Op) Token], [Token])
parseBracket [] = ([], [])
parseBracket (LeftBracket:toks) = (Node Nothing nested : rest', toks')
    where
        (nested, rest) = parseBracket toks
        (rest', toks') = parseBracket rest
parseBracket (RightBracket:toks) = ([], toks)
parseBracket (other:toks) = (Leaf other : rest, toks')
    where
        (rest, toks') = parseBracket toks


parseBrackets :: [Token] -> Tree (Maybe Op) Token
parseBrackets = Node Nothing . fst . parseBracket 


recurse :: ([Tree (Maybe a) l] -> Tree (Maybe a) l) -> Tree (Maybe a) l -> Tree (Maybe a) l
recurse _ (Leaf t) = Leaf t
recurse f (Node (Just x) children) = Node (Just x) $ map (recurse f) children
recurse f (Node Nothing children) = f $ map (recurse f) children

splitOn :: Eq a => a -> [a] -> Maybe ([a], [a])
splitOn val [] = Nothing
splitOn val (x:xs) = if x == val 
                   then Just ([], xs) 
                   else first (x:) <$> splitOn val xs 

parseOp :: Token -> Op -> Tree (Maybe Op) Token -> Tree (Maybe Op) Token
parseOp token op = recurse parseOp'
    where 
        parseOp' trees = case splitOn (Leaf token) trees of
            Just (before, after) -> Node (Just op) [ parseOp' before, parseOp' after ]
            Nothing -> Node Nothing trees

parseAddition :: Tree (Maybe Op) Token -> Tree (Maybe Op) Token
parseAddition = parseOp TokAdd Add

parseMultiplication :: Tree (Maybe Op) Token -> Tree (Maybe Op) Token
parseMultiplication = parseOp TokMul Mul

normTree :: Tree (Maybe a) b -> Maybe (Tree (Maybe a) b)
normTree (Leaf l) = Just $ Leaf l
normTree (Node (Just op) children) = Just $ Node (Just op) $ mapMaybe normTree children
normTree (Node Nothing children) = case mapMaybe normTree children of
    [] -> Nothing
    [t] -> Just t
    ts -> Just $ Node Nothing ts

compile :: Tree (Maybe Op) Token -> Expression
compile (Leaf (Num num)) = Constant num
compile (Node (Just op) [x, y]) = Calculation (compile x) op (compile y)

parseExpression :: [Token] -> Expression
parseExpression = compile . fromJust . normTree . parseAddition . parseMultiplication . parseBrackets

main :: IO ()
main = do
    ls <- fmap lines getContents
    mapM_ (print . resolve . parseExpression . tokenize) ls
    print $ sum $ map (resolve . parseExpression. tokenize) ls
