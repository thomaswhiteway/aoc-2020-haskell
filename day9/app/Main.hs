module Main where

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Sequence as Sequence
import Data.Sequence( ViewL((:<)), (|>) )
import Data.Maybe (fromMaybe, fromJust)
import Data.List (find)
import Data.Hashable
import System.Environment (getArgs)

type Counts a = HashMap.HashMap a Int

data Range = Range { total :: Int,
                     numbers :: Sequence.Seq Int }

emptyRange :: Range
emptyRange = Range { total = 0, numbers = Sequence.empty }

append :: Int -> Range -> Range
append val Range{total=t,numbers=ns} = Range { total = t + val, numbers = ns |> val }

dropFirst :: Range -> Range
dropFirst Range{total=t,numbers=ns} = Range { total = t - drop, numbers = ns' }
    where
        drop :< ns' = Sequence.viewl ns

reduceTo :: Int -> Range -> Range
reduceTo target range
    | total range > target = reduceTo target $ dropFirst range
    | otherwise            = range

weakness :: Range -> Int
weakness Range{numbers=ns} = maximum ns + minimum ns

emptyCounts :: Counts a
emptyCounts = HashMap.empty

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
        valid = emptyCounts `withAll` sums prefix

findInvalid' :: Sequence.Seq Int -> Counts Int -> [Int] -> Maybe Int
findInvalid' _    _     []     = Nothing
findInvalid' prev valid (v:vs) 
    | valid `contains` v = findInvalid' (prev' |> v) valid' vs
    | otherwise          = Just v
    where
        drop :< prev' = Sequence.viewl prev
        valid' = (valid `withoutAll` (fmap (+drop) prev')) `withAll` (fmap (+v) prev') 

findWeakness :: Int -> [Int] -> Maybe Int
findWeakness = findWeakness' emptyRange

findWeakness' :: Range -> Int -> [Int] -> Maybe Int
findWeakness' _     _      []     = Nothing
findWeakness' range target (v:vs) 
    | total range == target = Just $ weakness range
    | otherwise             = findWeakness' range' target vs
    where
        range' = reduceTo target $ append v range

main :: IO ()
main = do
    prefix <- fmap (read . head) getArgs
    nums <- fmap parse getContents
    invalid <- return $ findInvalid prefix nums
    case invalid of 
        Nothing -> putStrLn "All numbers are valid"
        Just invalid -> do
            putStrLn $ "invalid = " ++ show invalid
            weakness <- return $ findWeakness invalid nums
            putStrLn $ case weakness of 
                Nothing -> "No weakness"
                Just weakness -> "weakness = " ++ show weakness