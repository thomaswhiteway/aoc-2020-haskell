module Main where

import Text.Parsec (parse, digit, string, newline, char, many1, char, (<|>), endBy, ParsecT, eof)
import Data.List ( find )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Bits ( Bits((.|.), (.&.)), shift, testBit)
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap

type Signature = (Int, Int, Int, Int)
type TileId = Int

square = char '#' <|> char '.'

tileId = string "Tile " *> (read <$> many1 digit) <* string ":\n"
tileMap = many1 square `endBy` newline
tile = do
    id <- tileId
    map <- tileMap
    return (id, map)

tiles = tile `endBy` newline

parseTiles :: String -> [(TileId, [String])]
parseTiles text = case parse (tiles <* eof) "" text of 
    Left e -> error $ "Failed to parse tiles: " ++ show e
    Right ts -> ts

fromBinary :: [Int] -> Int
fromBinary [] = 0
fromBinary (x:xs) = (x `shift` length xs) .|. fromBinary xs

toBinary :: Int -> [Int]
toBinary x = [ if testBit x i then 1 else 0 | i <- reverse [0..9]]

sig :: String -> Int
sig line = fromBinary [if c == '#' then 1 else 0 | c <- line]

getSides :: [String] -> Signature
getSides tile = (sig $ head tile,
                 sig $ map last tile,
                 sig $ last tile,
                 sig $ map head tile)

flip :: Int -> Int
flip = fromBinary . reverse . toBinary

flipVertical :: Signature -> Signature
flipVertical (n, e, s, w) = (s, Main.flip e, n, Main.flip w)

flipHorizontal :: Signature -> Signature
flipHorizontal (n, e, s, w) = (Main.flip n, w, Main.flip s, e)

flips :: Signature -> [Signature]
flips x = [x, flipHorizontal x, flipVertical x, flipHorizontal $ flipVertical x]

rotate :: Signature -> Signature
rotate (n, e, s, w) = (w, n, e, s)

rotateN :: Int -> Signature -> Signature
rotateN 0 x = x
rotateN n x = rotateN (n-1) $ rotate x

rotations :: Signature -> [Signature]
rotations x = [ rotateN i x | i <- [0..3] ]

getPermutations :: Signature -> [Signature]
getPermutations sig = [ p | r <- rotations sig, p <- flips r ]

sqrt :: Int -> Int
sqrt x = fromJust $ find (\y -> y*y == x) [1..]

insert :: Eq a => a -> [a] -> [a]
insert x xs = if x `elem` xs then xs else x:xs

buildSigMap :: IntMap [Signature] -> IntMap [TileId]
buildSigMap sigs = foldr (\(tileid, sig) m -> IntMap.alter (Just . insert tileid . fromMaybe []) sig m) IntMap.empty
    [ (tileid, d) | (tileid, sigs') <- IntMap.toList sigs, (n, e, s, w) <- sigs', d <- [n, e, s, w] ]

isCorner :: IntMap [TileId] -> TileId -> Signature -> Bool
isCorner sigMap tid (n, e, s, w) = (isEdge tid n && isEdge tid e) || (isEdge tid e && isEdge tid s) || (isEdge tid s && isEdge tid w) || (isEdge tid w && isEdge tid n)
    where
        isEdge :: TileId -> Int -> Bool
        isEdge tid d = length (sigMap ! d) == 1

findCorners :: IntMap [Signature] -> IntMap [TileId] -> [TileId]
findCorners tiles sigMap = [ tileid | (tileid, sigs) <- IntMap.toList tiles
                                    , all (isCorner sigMap tileid) sigs ]

main :: IO ()
main = do
    ts <- fmap parseTiles getContents 
    print $ length ts
    let sideLength = Main.sqrt $ length ts
    print sideLength
    let ts' = IntMap.fromList [ (index, getPermutations $ getSides tile) | (index, tile) <- ts ]
    let sigMap = buildSigMap ts'
    let corners = findCorners ts' sigMap
    putStrLn $ "Have " ++ show (length corners) ++ " corners"
    print $ product corners