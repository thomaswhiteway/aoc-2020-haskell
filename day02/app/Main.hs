module Main where

import Text.Parsec (many, digit, char, anyChar, string, parse, ParsecT);
import Util (count);
import Data.Either (fromRight);

data Rule = Rule { 
    lower :: Int,
    upper :: Int,
    character :: Char
} 

xor :: Bool -> Bool -> Bool
xor a b = a /= b

satisfies :: Rule -> String -> Bool
satisfies Rule { lower, upper, character } password = ((password !! (lower - 1)) == character) `xor` ((password !! (upper - 1)) == character)

range = do
    lower <- many digit 
    char '-'
    upper <- many digit
    return (read lower, read upper)

rule = do
    (lower, upper) <- range
    char ' '
    character <- anyChar
    return Rule { lower, upper, character }

password = many anyChar

entry = do
    r <- rule
    string ": "
    p <- password
    return (r, p)

solve :: String -> Bool
solve line = satisfies r p
    where 
        (r, p) = fromRight (error "Failed to parse entry") $ parse entry "" line 

main :: IO ()
main = interact (show . count id . map solve . lines)
