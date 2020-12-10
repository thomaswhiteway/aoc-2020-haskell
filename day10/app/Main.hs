module Main where

import Data.List (sort)

parse :: String -> [Int]
parse = map read . lines

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

formChain :: [Int] -> [Int]
formChain adapters = 0 : sort adapters ++ [maximum adapters + 3]

getDifferences :: [Int] -> [Int]
getDifferences chain = zipWith (-) (tail chain) chain

segment :: [Int] -> [[Int]]
segment []    = []
segment chain = seg : segment rest
    where
        (seg, rest) = nextSegment chain

nextSegment :: [Int] -> ([Int], [Int])
nextSegment []        = undefined
nextSegment [x]       = ([x], [])
nextSegment (x:y:rest) 
    | y - x == 3 = ([x], y:rest)
    | otherwise  = (x:seg, rest')
    where
        (seg, rest') = nextSegment (y:rest)

countPossibilities :: [Int] -> Int
countPossibilities []         = undefined
countPossibilities [_]        = 1
countPossibilities (x:xs) = countPossibilities' x xs
    where
        countPossibilities' _    []         = undefined
        countPossibilities' _    [_]        = 1
        countPossibilities' last (x:y:rest) 
            | y - last > 3 = countPossibilities' x (y:rest)
            | otherwise    = countPossibilities' x (y:rest) + countPossibilities' last (y:rest)
 
main :: IO ()
main = do 
    adapters <- fmap parse getContents
    let chain = formChain adapters
    let diffs = getDifferences chain
    let onejolt = count (==1) diffs
    putStrLn $ show onejolt ++ " 1-jolt differences"
    let threejolt = count (==3) diffs
    putStrLn $ show threejolt ++ " 3-jolt differences"
    putStrLn $ "Answer = " ++ show (onejolt * threejolt)
    let segments = segment chain
    putStrLn $ "Have " ++ show (length segments) ++ " segments, maximum length = " ++ show (maximum $ map length segments)
    let possibilities = product $ map countPossibilities segments
    putStrLn $ "There are " ++ show possibilities ++ " possibile arrangements"