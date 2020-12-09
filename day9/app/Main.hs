module Main where

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Sequence as Sequence
import Data.Sequence( ViewL((:<)), (|>) )
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Hashable
import System.Environment (getArgs)

type Counts a = HashMap.HashMap a Int

empty :: Counts a
empty = HashMap.empty

contains :: (Eq a, Hashable a) => Counts a -> a -> Bool
contains counts item = HashMap.lookupDefault 0 item counts > 0

withAll :: (Eq a, Hashable a, Foldable t) => Counts a -> t a -> Counts a
withAll = foldr with

with :: (Eq a, Hashable a) => a -> Counts a -> Counts a
with = HashMap.alter (\v -> Just $ fromMaybe 0 v + 1)

withoutAll :: (Eq a, Hashable a, Foldable t) => Counts a -> t a -> Counts a
withoutAll = foldr without 

without :: (Eq a, Hashable a) => a -> Counts a -> Counts a
without = HashMap.update (\v -> if v == 1 then Nothing else Just (v - 1))

parse :: String -> [Int]
parse = map read . lines

sums :: [Int] -> [Int]
sums []     = []
sums (x:xs) = map (+x) xs ++ sums xs

findInvalid :: Int -> [Int] -> Maybe Int
findInvalid num nums = findInvalid' (Sequence.fromList prefix) valid remainder
    where
        (prefix, remainder) = splitAt num nums
        valid = empty `withAll` sums prefix

findInvalid' :: Sequence.Seq Int -> Counts Int -> [Int] -> Maybe Int
findInvalid' _    _     []     = Nothing
findInvalid' prev valid (v:vs) 
    | valid `contains` v = findInvalid' (prev' |> v) valid' vs
    | otherwise          = Just v
    where
        drop :< prev' = Sequence.viewl prev
        valid' = (valid `withoutAll` (fmap (+drop) prev')) `withAll` (fmap (+v) prev') 

main :: IO ()
main = do
    len <- fmap (read . head) getArgs
    interact (fromMaybe "All Valid" . fmap show . findInvalid len . parse)
