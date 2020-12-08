module Main where

import Text.Parsec (parse, char, string, choice, try, many1, digit, endBy, newline, eof, ParsecT)
import Data.Functor.Identity ( Identity )
import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust)
import Maybes (firstJusts)

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

step :: Program -> State -> Maybe State
step prog state 
    | pc state == length prog = Nothing
    | otherwise               = Just $ execute (prog !! pc state) state

run :: Program -> Maybe Int
run prog = run' IntSet.empty initialState
    where 
        next = step prog
        run' seen state
            | pc state `IntSet.member` seen = Nothing
            | otherwise                     = case next state of 
                Just newState -> run' (pc state `IntSet.insert` seen) newState
                Nothing       -> Just $ acc state

changes :: Program -> [Program]
changes = changes' [] 
    where 
        changes' _ [] = []
        changes' prefix (i:is) = case i of 
            Nop x -> update (Jmp x):rest
            Jmp x -> update (Nop x):rest
            Acc _ -> rest
            where
                update i' = prefix ++ (i':is)
                rest = changes' (prefix ++ [i]) is

solve :: Program -> Int
solve prog = fromJust $ firstJusts $ map run $ changes prog

main :: IO ()
main = interact (show . solve . parseProgram)
