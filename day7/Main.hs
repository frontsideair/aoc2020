{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Data.Either
import Data.Functor
import Data.Map.Strict hiding (mapMaybe)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Text
import Prelude hiding (null)

shinyGold = "shiny gold"

main :: IO ()
main = do
  parsed <- parseFromFile inputFile "./input"
  let rules = fromRight [] parsed
  let digraph = fromListWith (++) [(key, [value]) | (value, keys) <- rules, (key, _) <- keys]
  let results = search digraph [shinyGold]
  print $ Set.size results - 1
  let digraph' = fromList rules
  print $ search' digraph' shinyGold - 1
  return ()

search digraph colors = if Set.null foundColors then colors else search digraph' (colors `Set.union` foundColors)
  where
    foundColors = Set.fromList [x | color <- Set.toList colors, xs <- maybeToList (digraph !? color), x <- xs]
    digraph' = withoutKeys digraph colors

search' digraph color = maybe 1 f (digraph !? color)
  where
    f colors = sum [count * search' digraph color | (color, count) <- colors] + 1

inputFile = do
  result <- many line
  eof
  return result

line = do
  source <- color
  space
  string "bags contain"
  space
  destination <- destColor `sepBy1` string ", " <|> string "no other bags" $> []
  char '.'
  endOfLine
  return (source, destination)

color = mconcat [many letter, string " ", many letter]

destColor = do
  num <- many digit
  space
  c <- color
  space
  many letter
  return (c, read num :: Int)