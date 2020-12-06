module Main where

import Text.Parsec (parse, sepBy, endBy, newline, many1, lower, eof)
import qualified Data.IntSet as IntSet

type Group = [String]

person = many1 lower
group = person `endBy` newline
groups = group `sepBy` newline

parseGroups :: String -> [Group]
parseGroups text = case parse (groups <* eof) "" text of
    Left e -> error ("Failed to parse groups: " ++ show e)
    Right g -> g

totalQuestions :: Group -> Int
totalQuestions = IntSet.size . IntSet.fromList . map fromEnum . concat 

commonQuestions :: Group -> Int
commonQuestions = IntSet.size . foldl1 IntSet.intersection . map (IntSet.fromList . map fromEnum)

main :: IO ()
main = interact (show . sum . map commonQuestions . parseGroups)
