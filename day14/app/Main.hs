{-# LANGUAGE TupleSections #-}
module Main where

import Text.Parsec (parse, many, char, string, (<|>), many1, digit, endBy, newline, eof, try)
import Data.Bits ( Bits((.|.), (.&.)), shift )
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

data BitMask = ValueMask Int Int | AddressMask Int [Int] deriving (Show)
type Memory = IntMap Int
data MemState = MemState BitMask Memory
data Command = MemSet Int Int | SetMask String

toBinary :: [Int] -> Int
toBinary [] = 0
toBinary (x:xs) = (x `shift` length xs) .|. toBinary xs

toBinaryOptions :: [[Int]] -> [Int]
toBinaryOptions [] = [0]
toBinaryOptions (x:xs) = [x' `shift` length xs .|. xs' | x' <- x, xs' <- toBinaryOptions xs]

bitmask = do 
    string "mask = "
    SetMask <$> many (char '0' <|> char '1' <|> char 'X')

valueMask :: String -> BitMask
valueMask bits = ValueMask mask set
    where
        mask = toBinary [if b == 'X' then 1 else 0 | b <- bits]
        set = toBinary [if b == '1' then 1 else 0 | b <- bits]

addressMask :: String -> BitMask 
addressMask bits = AddressMask mask values
    where
        mask = toBinary [if b == '0' then 1 else 0 | b <- bits]
        values = toBinaryOptions [case b of 
            '0' -> [0]
            '1' -> [1]
            'X' -> [0, 1] | b <- bits
         ]

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
initialState = MemState (ValueMask 0 0) IntMap.empty

apply :: BitMask -> Int -> Int -> ([Int], Int)
apply (ValueMask   mask bits) address value = ([address], (value .&. mask) .|. bits)
apply (AddressMask mask bits) address value = ([(address .&. mask) .|. bits' | bits' <- bits], value)

setMemory :: MemState -> Int -> Int -> MemState
setMemory (MemState mask memory) address value = MemState mask $ foldr (uncurry IntMap.insert . (, value')) memory addresses
    where
        (addresses, value') = apply mask address value

execute :: (String -> BitMask) -> MemState -> Command -> MemState
execute maskBuilder (MemState _ memory) (SetMask mask)         = MemState (maskBuilder mask) memory
execute _           state               (MemSet address value) = setMemory state address value

run :: (String -> BitMask) -> MemState -> [Command] ->  MemState
run maskBuilder = foldl (execute maskBuilder)

main :: IO ()
main = do
    program <- fmap parseProgram getContents
    let MemState _ valueEndState = run valueMask initialState program
    putStrLn $ "With value mask -> " ++ show (sum valueEndState)
    let MemState _ addressEndState = run addressMask initialState program
    putStrLn $ "With address mask -> " ++ show (sum addressEndState)