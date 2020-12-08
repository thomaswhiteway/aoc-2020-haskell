module Main where

import Text.Parsec (parse, char, string, choice, try, many1, digit, oneOf, endBy, newline, eof, ParsecT)
import Data.Functor.Identity ( Identity )
import qualified Data.IntSet as IntSet

data Instruction = Nop Int | Acc Int | Jmp Int

type Program = [Instruction]
data State = State { pc :: Int, acc :: Int }

initialState = State { pc = 0, acc = 0 }

operation = choice $ map optionParser [("nop", Nop),
                                       ("acc", Acc),
                                       ("jmp", Jmp)]

optionParser :: (String, t) -> ParsecT String u Identity t
optionParser (token, value) = do { try $ string token; return value }

argument :: ParsecT String u Identity Int
argument = do 
    s <- sign
    val <- many1 digit
    return $ s * read val

sign :: ParsecT String u Identity Int
sign = choice $ map optionParser [("+",  1),
                                  ("-", -1)]

instruction :: ParsecT String u Identity Instruction
instruction = do
    o <- operation
    char ' '
    o <$> argument

program = instruction `endBy` newline

parseProgram :: String -> Program
parseProgram text = case parse (program <* eof) "" text of
    Left e -> error ("Failed to parse program " ++ show e)
    Right p -> p

execute :: Instruction -> State -> State
execute (Nop _) (State pc acc) = State (pc+1) acc
execute (Acc x) (State pc acc) = State (pc+1) (acc+x)
execute (Jmp x) (State pc acc) = State (pc+x) acc

step :: Program -> State -> State
step prog state = execute (prog !! pc state) state

solve :: Program -> Int
solve prog = solve' IntSet.empty initialState
    where 
        next = step prog
        solve' seen state
            | pc state `IntSet.member` seen = acc state
            | otherwise               = solve' (pc state `IntSet.insert` seen) (next state)

main :: IO ()
main = interact (show . solve . parseProgram)
