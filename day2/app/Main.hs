module Main where

import Text.Parsec (many, digit, char, anyChar, string, parse, ParsecT);
import Util (count);

data Rule = Rule { 
    lower :: Int,
    upper :: Int,
    character :: Char
} 

satisfies :: Rule -> String -> Bool
satisfies Rule { lower, upper, character } password = appearances >= lower && appearances <= upper
    where
        appearances = count (==character) password

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
        (r, p) = case parse entry "" line of
            Left err -> error "Failed to parse entry"
            Right (r, p) -> (r, p)


main :: IO ()
main = interact (show . count id . map solve . lines)
