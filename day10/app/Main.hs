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