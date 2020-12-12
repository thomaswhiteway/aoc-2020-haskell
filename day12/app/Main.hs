module Main where

import Text.Parsec (parse, eof, newline, endBy, try, string, choice, ParsecT, many1, digit)
import Data.Functor.Identity ( Identity )

data Instruction = Instruction Action Int deriving (Show)
data Action = MoveNorth | MoveSouth | MoveEast | MoveWest | TurnLeft | TurnRight | MoveForward deriving (Show)
data Direction = North | East | South | West deriving (Show)
data Boat = Boat (Int, Int) Direction deriving (Show)

optionParser :: (String, t) -> ParsecT String u Identity t
optionParser (token, value) = do { try $ string token; return value }

action = choice $ map optionParser [("N", MoveNorth),
                                    ("S", MoveSouth),
                                    ("E", MoveEast),
                                    ("W", MoveWest),
                                    ("L", TurnLeft),
                                    ("R", TurnRight),
                                    ("F", MoveForward)]

argument = read <$> many1 digit

instruction = do
    a <- action
    Instruction a <$> argument

instructions :: ParsecT String u Identity [Instruction]
instructions = instruction `endBy` newline

parseInstructions :: String -> [Instruction]
parseInstructions text = case parse (instructions <* eof) "" text of
    Left e -> error $ "Failed to parse instructions " ++ show e
    Right prog -> prog

directionAction :: Direction -> Action
directionAction North = MoveNorth
directionAction East  = MoveEast
directionAction South = MoveSouth
directionAction West  = MoveWest

moveInDirection :: Direction -> Int -> Instruction
moveInDirection d = Instruction $ directionAction d

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnLeftBy :: Int -> Direction -> Direction
turnLeftBy 0 d = d
turnLeftBy x d = turnLeftBy (x-90) (turnLeft d)

turnRightBy :: Int -> Direction -> Direction
turnRightBy x = turnLeftBy (360-x)

updateBoat :: Instruction -> Boat -> Boat
updateBoat (Instruction MoveNorth   v) (Boat (x, y) d) = Boat (x,   y+v) d
updateBoat (Instruction MoveSouth   v) (Boat (x, y) d) = Boat (x,   y-v) d
updateBoat (Instruction MoveEast    v) (Boat (x, y) d) = Boat (x+v, y  ) d
updateBoat (Instruction MoveWest    v) (Boat (x, y) d) = Boat (x-v, y  ) d
updateBoat (Instruction TurnLeft    v) (Boat (x, y) d) = Boat (x,   y  ) (turnLeftBy  v d)
updateBoat (Instruction TurnRight   v) (Boat (x, y) d) = Boat (x,   y  ) (turnRightBy v d)
updateBoat (Instruction MoveForward v) (Boat (x, y) d) = updateBoat (moveInDirection d v) (Boat (x, y) d)

executeOnBoat :: [Instruction] -> Boat -> Boat
executeOnBoat prog b = foldl (flip updateBoat) b prog

main :: IO ()
main = do
    prog <- fmap parseInstructions getContents
    let Boat (x, y) _ = executeOnBoat prog $ Boat (0, 0) East
    putStrLn $ "End at (" ++ show x ++ ", " ++ show y ++ "), distance " ++ show (abs x + abs y)
