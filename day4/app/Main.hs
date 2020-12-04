module Main where

import Text.Parsec (many, char, parse, sepBy, space, noneOf, (<|>), string, try, endBy, ParsecT, digit, count, oneOf, eof, choice); 
import Data.Functor.Identity ( Identity );
import Text.Read (readMaybe);
import Data.Either (isRight)

data PassportEntryType = 
      BirthYear 
    | IssueYear 
    | ExpirationYear 
    | Height 
    | HairColor 
    | EyeColor 
    | PassportId 
    | CountryId deriving (Eq, Show)

requiredTypes = [
    BirthYear,
    IssueYear,
    ExpirationYear,
    Height,
    HairColor,
    EyeColor,
    PassportId]

data HeightUnit = 
      Centimetre
    | Inch

type Passport = [(PassportEntryType, String)]

passportEntryType = choice $ map (uncurry specificType) [("byr", BirthYear),
                                                         ("iyr", IssueYear),
                                                         ("eyr", ExpirationYear),
                                                         ("hgt", Height),
                                                         ("hcl", HairColor),
                                                         ("ecl", EyeColor),
                                                         ("pid", PassportId),
                                                         ("cid", CountryId)]

specificType :: String -> PassportEntryType -> ParsecT String u Identity PassportEntryType
specificType token t = do { try $ string token; return t }

passportEntry :: ParsecT String u Identity (PassportEntryType, [Char])
passportEntry = do
    t <- passportEntryType
    char ':'
    v <- many $ noneOf "\n "
    return (t, v)

passport :: ParsecT String u Identity [(PassportEntryType, [Char])]
passport = passportEntry `endBy` space

passports = passport `sepBy` string "\n"

parsePassports :: String -> [Passport]
parsePassports text = case parse passports "" text of
    Left e -> error ("Failed to parse passports: " ++ show e)
    Right ps -> ps

validYear :: Int -> Int -> String -> Bool
validYear min max text = length text == 4 && case readMaybe text of 
    Just year -> year >= min && year <= max
    Nothing -> False

heightUnit = do { try $ string "cm"; return Centimetre }
         <|> do { try $ string "in"; return Inch }

height :: ParsecT String u Identity (HeightUnit, Integer)
height = do 
    v <- read <$> many digit 
    unit <- heightUnit
    eof
    return (unit, v)

validHeight :: String -> Bool
validHeight text = case parse height "" text of 
    Left _ -> False
    Right (Centimetre, h) -> 150 <= h && h <= 193
    Right (Inch, h) -> 59 <= h && h <= 76

hairColor = char '#' *> count 6 (oneOf "0123456789abcdef") <* eof

validHairColor :: String -> Bool
validHairColor = isRight . parse hairColor "" 

eyeColor = choice (map (try . string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) <* eof

validEyeColor :: String -> Bool
validEyeColor = isRight . parse eyeColor ""

passportId = count 9 digit <* eof

validPassportId :: String -> Bool
validPassportId = isRight . parse passportId ""

entryValid :: (PassportEntryType, String) -> Bool
entryValid (BirthYear, v) = validYear 1920 2002 v
entryValid (IssueYear, v) = validYear 2010 2020 v
entryValid (ExpirationYear, v) = validYear 2020 2030 v
entryValid (Height, v) = validHeight v
entryValid (HairColor, v) = validHairColor v
entryValid (EyeColor, v) = validEyeColor v
entryValid (PassportId, v) = validPassportId v
entryValid (CountryId, _) = True


isValid :: Passport -> Bool
isValid passport = all (`elem` entryTypes) requiredTypes && all entryValid passport
    where
        entryTypes = [t | (t, _) <- passport]

main :: IO ()
main = interact (show . length . filter isValid . parsePassports)
