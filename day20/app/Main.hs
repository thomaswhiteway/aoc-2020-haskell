module Main where

import Text.Parsec (parse, digit, string, newline, char, many1, char, (<|>), endBy, ParsecT, eof)
import Data.List ( find, delete, intercalate )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Bits ( Bits((.|.), (.&.)), shift, testBit)
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap
import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as Map
import Debug.Trace (trace)
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

parseTiles :: String -> IntMap [String]
parseTiles text = case parse (tiles <* eof) "" text of 
    Left e -> error $ "Failed to parse tiles: " ++ show e
    Right ts -> IntMap.fromList ts

fromBinary :: [Int] -> Int
fromBinary [] = 0
fromBinary (x:xs) = (x `shift` length xs) .|. fromBinary xs

toBinary :: Int -> [Int]
toBinary x = [ if testBit x i then 1 else 0 | i <- reverse [0..9]]

sig :: String -> Int
sig line = fromBinary [if c == '#' then 1 else 0 | c <- line]

getSignature :: [String] -> Signature
getSignature tile = (sig $ head tile,
                 sig $ map last tile,
                 sig $ last tile,
                 sig $ map head tile)

north :: Signature -> Int
north (n, _, _, _) = n

east :: Signature -> Int
east (_, e, _, _) = e

south :: Signature -> Int
south (_, _, s, _) = s

west :: Signature -> Int
west (_, _, _, w) = w

sqrt :: Int -> Int
sqrt x = fromJust $ find (\y -> y*y == x) [1..]

insert :: Eq a => a -> [a] -> [a]
insert x xs = if x `elem` xs then xs else x:xs

buildedgeToTileIds :: IntMap [Signature] -> IntMap [TileId]
buildedgeToTileIds sigs = foldr (\(tileid, sig) m -> IntMap.alter (Just . insert tileid . fromMaybe []) sig m) IntMap.empty
    [ (tileid, d) | (tileid, sigs') <- IntMap.toList sigs, (n, e, s, w) <- sigs', d <- [n, e, s, w] ]

isCorner :: IntMap [TileId] -> TileId -> Signature -> Bool
isCorner edgeToTileIds tid (n, e, s, w) = any (\(d1, d2) -> isEdge' d1 && isEdge' d2) [(n, e), (e, s), (s, w), (w, n)]
    where
        isEdge' = isEdge edgeToTileIds tid

isEdge :: IntMap [TileId] -> TileId -> Int -> Bool
isEdge edgeToTileIds tid d = length (edgeToTileIds ! d) == 1

findCorners :: IntMap [Signature] -> IntMap [TileId] -> [TileId]
findCorners tiles edgeToTileIds = [ tileid | (tileid, sigs) <- IntMap.toList tiles
                                           , all (isCorner edgeToTileIds tileid) sigs ]

indent :: Int -> String -> String
indent n text =  replicate (n * 2) ' ' ++ text

placeTiles :: (Int, Int) -> IntMap [Signature] -> IntMap [TileId] -> [Map (Int, Int) (TileId, Signature)]
placeTiles (width, height) tileIdToSigs edgeToTileIds = placeTiles' Map.empty (IntMap.keys tileIdToSigs) [(x, y) | y <- [0..width-1], x <- [0..height-1]]
    where

        placeTiles' :: Map (Int, Int) (TileId, Signature) -> [TileId] -> [(Int, Int)] -> [Map (Int, Int) (TileId, Signature)]
        placeTiles' current []      []         = [current]
        placeTiles' current tileids (pos@(x, y):poss) = [ sol | (tileid, sig) <- possibleTiles current tileids pos 
                                                              , sol <- placeTiles' (placeTile tileid sig pos current) (delete tileid tileids) poss ]

        placeTile :: TileId -> Signature -> (Int, Int) -> Map (Int, Int) (TileId, Signature) -> Map (Int, Int) (TileId, Signature) 
        placeTile tileid sig pos m =  Map.insert pos (tileid, sig) m
            
        possibleTiles :: Map (Int, Int) (TileId, Signature) -> [TileId] -> (Int, Int) -> [(TileId, Signature)]                                                       
        possibleTiles current tileids pos = [ (tileid, sig) | tileid <- tileids
                                                            , sig <- tileIdToSigs ! tileid 
                                                            , fits current pos tileid sig ]

        fits current pos tileid sig = all (\f -> f current pos tileid sig) [fitsN, fitsE, fitsS, fitsW]
        fitsN current  (x, y) tileid (n, _, _, _)
            | y == 0          = isEdge edgeToTileIds tileid n
            | otherwise       = maybe True (\(_, sig) -> n == south sig) (current !? (x, y-1))
        fitsE current (x, y) tileid (_, e, _, _) 
            | x == (width-1)  = isEdge edgeToTileIds tileid e
            | otherwise       = maybe True (\(_, sig) -> e == west  sig) (current !? (x+1, y))
        fitsS current  (x, y) tileid (_, _, s, _)
            | y == (height-1) = isEdge edgeToTileIds tileid s
            | otherwise       = maybe True (\(_, sig) -> s == north sig) (current !? (x, y+1))
        fitsW current (x, y) tileid (_, _, _, w) 
            | x == 0          = isEdge edgeToTileIds tileid w
            | otherwise       = maybe True (\(_, sig) -> w == east  sig) (current !? (x-1, y))            

rotate []           = []
rotate [line]       = map (:[]) $ reverse line
rotate (line:lines) = zipWith (:) (reverse line) (rotate lines) 

rotateN 0 square = square
rotateN n square = rotateN (n-1) $ rotate square

rotations square = [ rotateN n square | n <- [0..3] ]

flipHorizontal = map reverse
flipVertical = reverse

flips square = [ square, flipHorizontal square, flipVertical square, flipHorizontal $ flipVertical square ]

getPermutations :: [String] -> [[String]]
getPermutations square = [ sq | r <- rotations square, sq <- flips r ]

findPermutation :: [String] -> Signature -> [String]
findPermutation square sig = fromJust $ find (\sq -> getSignature sq == sig) $ getPermutations square

flatten :: [[[String]]] -> [String]
flatten = concatMap (foldr (zipWith (++)) (replicate 10 [])) 

renderMap :: (Int, Int) -> IntMap [String] -> Map (Int, Int) (TileId, Signature) -> [String]
renderMap (width, height) tiles positions = flatten [[ trimCrusts $ findTile (x, y) | x <- [0..width-1]] | y <- [0..height-1]]
    where
        findTile :: (Int, Int) -> [String]
        findTile pos = findPermutation (tiles ! tileid) sig
            where
                Just (tileid, sig) = positions !? pos
        trimCrusts :: [String] -> [String]
        trimCrusts = map (tail . init) . tail . init

seaMonster :: [String]
seaMonster = lines "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "

countPattern matcher [] = 0
countPattern matcher text@(_:rest) = countRowMatches matcher text + countPattern matcher rest

countRowMatches matcher text 
    | length text < length matcher = 0
    | length (head text) < length (head matcher) = 0
    | otherwise  = (if isMatch matcher text then 1 else 0) + countRowMatches matcher (map tail text)

isMatch matcher text = and $ zipWith (\m l -> and $ zipWith ($) m l) matcher text

buildChecker text = [[if c == ' ' then const True else (==c) | c <- line] | line <- text]

countSeaMonsters :: [String] -> Int
countSeaMonsters = countPattern (buildChecker seaMonster)

count f = length . filter f

main :: IO ()
main = do
    tiles <- fmap parseTiles getContents 
    print $ length tiles
    let side = Main.sqrt $ length tiles
    print side
    let tileIdToSigs = IntMap.map (map getSignature . getPermutations) tiles
    let edgeToTileIds = buildedgeToTileIds tileIdToSigs
    let corners = findCorners tileIdToSigs edgeToTileIds
    putStrLn $ "Have " ++ show (length corners) ++ " corners"
    print $ product corners
    case placeTiles (side, side) tileIdToSigs edgeToTileIds of
        [] -> putStrLn "No Solution"
        (m:_) -> do
            let display = renderMap (side, side) tiles m
            putStrLn $ unlines display
            let seaMonsters = maximum $ map countSeaMonsters $ getPermutations display
            putStrLn $ "Found " ++ show seaMonsters ++ " sea monsters\n"
            let roughness = count (=='#') (unlines display) - seaMonsters * count (=='#') (unlines seaMonster)
            putStrLn $ "Roughness " ++ show roughness