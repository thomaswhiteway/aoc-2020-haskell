module Main where

import Data.Array ( Array, (!))
import Data.Array.IO (IOUArray)
import Data.Array.MArray (MArray (getBounds) , readArray, writeArray, freeze)
import Data.Char (digitToInt)
import Data.List (sort, intercalate)
import qualified Data.Array as Array
import qualified Data.Array.MArray as MArray
import System.Environment (getArgs)
import Data.Foldable (toList)

findNext :: Int -> Int -> [Int] -> Int
findNext m 0 xs = findNext m m xs
findNext m    n xs 
    | n `elem` xs = findNext m (n-1) xs
    | otherwise   = n

insertAfter :: Int -> [Int] -> [Int] -> [Int]
insertAfter n toInsert (x:xs)
    | n == x    = (x : toInsert) ++ xs
    | otherwise = x : insertAfter n toInsert xs

follow :: Int -> IOUArray Int Int -> Int -> IO [Int]
follow 0 _ x = return []
follow n buffer x = do
    x' <- readArray buffer x
    rest <- follow (n-1) buffer x'
    return (x':rest)

step :: IOUArray Int Int -> Int -> IO Int
step buffer n = do
    [a, b, c, n'] <- follow 4 buffer n
    (_, m) <- getBounds buffer
    let insertPoint = findNext m (n-1) [a, b, c]
    after <- readArray buffer insertPoint
    writeArray buffer insertPoint a
    writeArray buffer c after
    writeArray buffer n n'
    return n'

buildPermutation :: [Int] -> IO (IOUArray Int Int)
buildPermutation elements = MArray.newListArray (1,upper) $ map snd $ sort $ zip elements (tail elements ++ [head elements])
    where
        upper = length elements

sequenceFrom :: Int -> Array Int Int -> [Int]
sequenceFrom start array = sequenceFrom' start
    where
        sequenceFrom' :: Int -> [Int]
        sequenceFrom' n = n : rest
            where
                n' = array ! n
                rest = if n' == start then [] else sequenceFrom' n' 

repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM 0 _ x = return x
repeatM n f x = do
    x' <- f x
    repeatM (n-1) f x'

main :: IO ()
main = do
    sequence <- map digitToInt . head <$> getArgs
    buffer <- buildPermutation sequence
    repeatM 100 (step buffer) (head sequence) 
    sequence' <- sequenceFrom 1 <$> freeze buffer
    putStrLn $ concatMap show $ tail sequence'
    buffer <- buildPermutation $ sequence ++ [10..1000000]
    repeatM 10000000 (step buffer) (head sequence)
    sequence' <- sequenceFrom 1 <$> freeze buffer
    print $ (sequence' !! 1) * (sequence' !! 2)


