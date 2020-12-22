{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Text
import Data.Bifunctor
import Data.Char (isLetter)
import Data.Foldable
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO as Text

main :: IO ()
main = do
  file <- Text.readFile "./input"
  let foods = case parseOnly parser file of
        Left err -> error err
        Right parsed -> parsed
  let ingredients = concat $ fst <$> foods
  let ingredientsSet = Set.fromList ingredients
  let allergens = fold $ snd <$> foods
  let allergensMap = Map.fromSet (const ingredientsSet) allergens
  let foodPairs = [(first Set.fromList food1, first Set.fromList food2) | food1 <- foods, food2 <- foods, food1 < food2]
  let allergensMap' = List.foldl' f allergensMap foodPairs
  let allergenIngredientsMap = myFold (Map.empty, Map.toList allergensMap')
  print $ length $ filter (`Set.notMember` Map.keysSet allergenIngredientsMap) ingredients
  print $ Text.intercalate "," $ fst <$> List.sortOn snd (Map.toList allergenIngredientsMap)
  return ()

f :: Map Text (Set Text) -> ((Set Text, Set Text), (Set Text, Set Text)) -> Map Text (Set Text)
f acc ((ingredients1, allergens1), (ingredients2, allergens2)) =
  Set.foldl' g acc commonAllergens
  where
    commonAllergens = Set.intersection allergens1 allergens2
    commonIngredients = Set.intersection ingredients1 ingredients2
    g acc allergen = Map.insertWith Set.intersection allergen commonIngredients acc

myFold (left, right) = case List.sortOn (\(k, v) -> length v) right of
  [] -> left
  (k, v) : rest -> myFold (Map.insert (Set.findMin v) k left, second (`Set.difference` v) <$> rest)

run1 input = input

parser = sepBy1 line endOfLine <* endOfLine <* endOfInput

line = do
  ingredients <- sepBy1 word (char ' ')
  string " (contains "
  allergens <- sepBy1 word (string ", ")
  string ")"
  return (ingredients, Set.fromList allergens)

word = takeWhile1 isLetter