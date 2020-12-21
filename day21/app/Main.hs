module Main where

import Text.Parsec (parse, letter, many1, sepBy, endBy, char, string, newline)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set, (\\))
import qualified Data.Set as Set
import Data.List.Unique (uniq)
import Data.List (intercalate, sort)

ingredient = many1 letter
allergen = many1 letter

ingredientList = ingredient `endBy` char ' '
allergenList = allergen `sepBy` string ", "

foodDetails = do
    ingredients <- ingredientList
    string "(contains "
    allergens <- allergenList
    string ")"
    return (ingredients, allergens)

foods = foodDetails `endBy` newline

parseFoods :: String -> [([String], [String])]
parseFoods text = case parse foods "" text of
    Left e -> error $ "Failed to parse foods: " ++ show e
    Right f -> f

getPotentialAllergens :: [([String], [String])] -> Map String (Set String)
getPotentialAllergens foods = resolveKnownAllergens $ Map.fromList [ (allergen, findIngredients allergen) | allergen <- allAllergens ]
    where
        allAllergens = uniq [ allergen | (_, allergens) <- foods, allergen <- allergens ]
        findIngredients allergen = foldl1 Set.intersection [ Set.fromList ingredients | (ingredients, allergens) <- foods, allergen `elem` allergens ]

resolveKnownAllergens :: Map String (Set String) -> Map String (Set String)
resolveKnownAllergens allergens
    | allergens == allergens' = allergens
    | otherwise               = resolveKnownAllergens allergens'
    where 
        knownAllergenIngredients = Set.fromList [ head $ Set.elems ingredients | (allergen, ingredients) <- Map.toList allergens, Set.size ingredients == 1 ]
        allergens' = Map.map (\ingredients -> if Set.size ingredients == 1 then ingredients else ingredients \\ knownAllergenIngredients) allergens
    
count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
    foods <- parseFoods <$> getContents 
    let potentialAllergens = getPotentialAllergens foods
    let allIngredients = Set.fromList [ ingredient | (ingredients, _) <- foods, ingredient <- ingredients ]
    let potentialAllergenIngredients = Set.unions $ Map.elems potentialAllergens
    let knownSafeIngredients = allIngredients \\ potentialAllergenIngredients
    let occurrences = sum [ count (`Set.member` knownSafeIngredients) ingredients | (ingredients, _) <- foods ]
    print occurrences
    putStrLn $ intercalate "," [ ingredient | (_, ingredient) <- sort [ (allergen, head $ Set.elems ingredients) | (allergen, ingredients) <- Map.toList potentialAllergens ]]