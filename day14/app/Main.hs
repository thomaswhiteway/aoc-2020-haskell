module Main where

import Text.Parsec (parse, many, char, string, (<|>), many1, digit, endBy, newline, eof, try)
import Data.Bits ( Bits((.|.), (.&.)), shift )
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

data BitMask = BitMask Int Int deriving (Show)
type Memory = IntMap Int
data MemState = MemState BitMask Memory
data Command = MemSet Int Int | SetMask BitMask

toBinary :: [Int] -> Int
toBinary [] = 0
toBinary (x:xs) = (x `shift` length xs) .|. toBinary xs

bitmask = do 
    string "mask = "
    bits <- many (char '0' <|> char '1' <|> char 'X')
    let mask = toBinary [if b == 'X' then 1 else 0 | b <- bits]
    let set = toBinary [if b == '1' then 1 else 0 | b <- bits]
    return $ SetMask $ BitMask mask set

assignment = do
    string "mem["
    address <- fmap read (many1 digit)
    string "] = "
    value <- fmap read (many1 digit)
    return $ MemSet address value

command = try bitmask <|> assignment
program = command `endBy` newline

parseProgram :: String -> [Command]
parseProgram text = case parse (program <* eof) "" text of
    Left e -> error $ "Failed to parse program: " ++ show e
    Right p -> p

initialState :: MemState
initialState = MemState (BitMask 0 0) IntMap.empty

apply :: BitMask -> Int -> Int
apply (BitMask mask bits) value = (value .&. mask) .|. bits

setMemory :: MemState -> Int -> Int -> MemState
setMemory (MemState mask memory) address value = MemState mask $ IntMap.insert address (apply mask value) memory

execute :: MemState -> Command -> MemState
execute (MemState _ memory) (SetMask mask)         = MemState mask memory
execute state               (MemSet address value) = setMemory state address value

run :: MemState -> [Command] ->  MemState
run = foldl execute

main :: IO ()
main = do
    program <- fmap parseProgram getContents
    let MemState _ endState = run initialState program
    putStr $ show $ sum endState
