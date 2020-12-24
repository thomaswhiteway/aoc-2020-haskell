{-# LANGUAGE FlexibleContexts #-}
module Main where

import Text.Parsec (parse, string, many1, endBy, newline, choice, try, ParsecT, Stream)
import Data.Functor.Identity (Identity)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Util (nTimes)

type Position = (Int, Int)

data Direction = East
               | SouthEast
               | SouthWest
               | West
               | NorthWest
               | NorthEast deriving (Show)

option :: Stream s m Char => String -> a -> ParsecT s u m a
option s v = try $ do { string s; return v }

direction :: ParsecT String u Identity Direction
direction = choice $ map (uncurry option) [ ("e",  East)
                                          , ("se", SouthEast)
                                          , ("sw", SouthWest)
                                          , ("w",  West)
                                          , ("nw", NorthWest)
                                          , ("ne", NorthEast)
                                          ]

allDirections :: [Direction] 
allDirections = [ East
                , SouthEast
                , SouthWest
                , West
                , NorthWest
                , NorthEast ]

directions :: ParsecT String u Identity [Direction]
directions = many1 direction

instructions :: ParsecT String u Identity [[Direction]]
instructions = directions `endBy` newline

parseInstructions :: String -> [[Direction]]
parseInstructions text = case parse instructions "" text of 
    Left e -> error $ "Failed to parse tiles: " ++ show e
    Right t -> t

offset :: Direction -> (Int, Int)
offset East      = (1,   0)
offset SouthEast = (1,  -1)
offset SouthWest = (0,  -1)
offset West      = (-1,  0)
offset NorthWest = (-1,  1)
offset NorthEast = (0,   1)

step :: Position -> Direction -> Position
step (x, y) d = (x + dx, y + dy)
    where
        (dx, dy) = offset d

follow :: [Direction] -> Position
follow = foldl step (0,0)

paintTile :: Set Position -> [Direction] -> Set Position
paintTile blackTiles directions 
    | position `Set.member` blackTiles = position `Set.delete` blackTiles
    | otherwise                        = position `Set.insert` blackTiles
    where
        position = follow directions
 
paintTiles :: [[Direction]] -> Set Position
paintTiles = foldl paintTile Set.empty 

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

neighbours :: Position -> [Position]
neighbours pos = [ step pos d | d <- allDirections ]

remainsBlack :: Set Position -> Position -> Bool
remainsBlack blackTiles pos = numBlackNeighbours > 0 && numBlackNeighbours <= 2
    where
        numBlackNeighbours = count (`Set.member` blackTiles) (neighbours pos) 

becomesBlack :: Set Position -> Position -> Bool
becomesBlack blackTiles pos = numBlackNeighbours == 2
    where
        numBlackNeighbours = count (`Set.member` blackTiles) (neighbours pos) 

afterDay :: Set Position -> Set Position
afterDay blackTiles = remainingBlackTiles `Set.union` newBlackTiles
    where
        remainingBlackTiles = Set.filter (remainsBlack blackTiles) blackTiles
        newBlackTiles = Set.filter (becomesBlack blackTiles) whiteTiles
        whiteTiles = Set.unions [Set.fromList $ neighbours pos | pos <- Set.toList blackTiles ] `Set.difference` blackTiles

main :: IO ()
main = do
    instructions <- parseInstructions <$> getContents 
    let startTiles = paintTiles instructions
    print $ Set.size startTiles
    let endTiles= nTimes 100 afterDay startTiles
    print $ Set.size endTiles
