module Main where

import Text.Parsec (parse, eof, newline, endBy, try, string, choice, ParsecT, many1, digit)
import Data.Functor.Identity ( Identity )

data Instruction = Instruction Action Int deriving (Show)
data Action = MoveNorth | MoveSouth | MoveEast | MoveWest | TurnLeft | TurnRight | MoveForward deriving (Show)
data Direction = North | East | South | West deriving (Show)
data Boat d = Boat (Int, Int) d deriving (Show)

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

rotateLeft :: (Int, Int) -> (Int, Int)
rotateLeft (x, y) = (-y, x)

rotateLeftBy :: Int-> (Int, Int) -> (Int, Int)
rotateLeftBy 0 p = p
rotateLeftBy x p = rotateLeftBy (x-90) (rotateLeft p)

rotateRightBy :: Int -> (Int, Int) -> (Int, Int)
rotateRightBy x = rotateLeftBy (360-x)

updateBoat :: Instruction -> Boat Direction -> Boat Direction
updateBoat (Instruction MoveNorth   v) (Boat (x, y) d) = Boat (x,   y+v) d
updateBoat (Instruction MoveSouth   v) (Boat (x, y) d) = Boat (x,   y-v) d
updateBoat (Instruction MoveEast    v) (Boat (x, y) d) = Boat (x+v, y  ) d
updateBoat (Instruction MoveWest    v) (Boat (x, y) d) = Boat (x-v, y  ) d
updateBoat (Instruction TurnLeft    v) (Boat (x, y) d) = Boat (x,   y  ) (turnLeftBy  v d)
updateBoat (Instruction TurnRight   v) (Boat (x, y) d) = Boat (x,   y  ) (turnRightBy v d)
updateBoat (Instruction MoveForward v) (Boat (x, y) d) = updateBoat (moveInDirection d v) (Boat (x, y) d)

updateWaypoint :: Instruction -> Boat (Int, Int) -> Boat (Int, Int)
updateWaypoint (Instruction MoveNorth   v) (Boat p (x, y)) = Boat p (x,   y+v)
updateWaypoint (Instruction MoveSouth   v) (Boat p (x, y)) = Boat p (x,   y-v)
updateWaypoint (Instruction MoveEast    v) (Boat p (x, y)) = Boat p (x+v, y  )
updateWaypoint (Instruction MoveWest    v) (Boat p (x, y)) = Boat p (x-v, y  )
updateWaypoint (Instruction TurnLeft    v) (Boat p d)      = Boat p (rotateLeftBy  v d)
updateWaypoint (Instruction TurnRight   v) (Boat p d)      = Boat p (rotateRightBy v d)
updateWaypoint (Instruction MoveForward v) (Boat (x, y) (dx, dy)) = Boat (x + v*dx, y + v*dy) (dx, dy)

execute :: (Instruction -> Boat a -> Boat a) -> [Instruction] -> Boat a -> Boat a
execute update prog b = foldl (flip update) b prog

main :: IO ()
main = do
    prog <- fmap parseInstructions getContents
    let Boat (x, y) _ = execute updateBoat prog $ Boat (0, 0) East
    putStrLn $ "Moving boat ends at (" ++ show x ++ ", " ++ show y ++ "), distance " ++ show (abs x + abs y)
    let Boat (x, y) _ = execute updateWaypoint prog $ Boat (0, 0) (10, 1)
    putStrLn $ "Moving waypoint ends at (" ++ show x ++ ", " ++ show y ++ "), distance " ++ show (abs x + abs y)
