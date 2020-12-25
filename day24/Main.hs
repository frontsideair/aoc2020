{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Representable.Store (Store (..), StoreT (..), experiment, runStore, store)
import Data.Attoparsec.Text
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Total.Map (TotalMap)
import qualified Data.Total.Map as TotalMap

data Direction = E | SE | SW | W | NW | NE deriving (Show, Eq, Ord, Enum, Bounded)

type Hex = Map Coord ()

data Coord = Coord Int Int deriving (Show, Eq, Ord)

size, dbSize :: Int
size = 200
dbSize = size * 2 + 1

instance Enum Coord where
  fromEnum (Coord x y) = ((x + size) * dbSize) + y + size
  toEnum n = Coord (x - size) (y - size)
    where
      (x, y) = n `quotRem` dbSize

instance Bounded Coord where
  minBound = Coord (- size) (- size)
  maxBound = Coord size size

rule g =
  if extract g
    then not (numBlackNeighbors == 0 || numBlackNeighbors > 2)
    else numBlackNeighbors == 2
  where
    neighborCoords coord = filter (withinCoord (minBound, maxBound)) $ move coord <$> enumerate
    neighbors = experiment neighborCoords g
    numBlackNeighbors = countTrue neighbors

mkStore :: Hex -> Store (TotalMap Coord) Bool
mkStore map = store lookup $ Coord 0 0
  where
    lookup coord = coord `Map.member` map

move (Coord x y) E = Coord (x + 2) y
move (Coord x y) SE = Coord (x + 1) (y -1)
move (Coord x y) SW = Coord (x -1) (y - 1)
move (Coord x y) W = Coord (x -2) y
move (Coord x y) NW = Coord (x -1) (y + 1)
move (Coord x y) NE = Coord (x + 1) (y + 1)

flipMU Nothing = Just ()
flipMU (Just ()) = Nothing

countTrue = length . filter id

within n (min, max) = n >= min && n <= max

withinCoord (Coord minX minY, Coord maxX maxY) (Coord x y) = x `within` (minX, maxX) && y `within` (minY, maxY)

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

main :: IO ()
main = do
  file <- Text.readFile "./input"
  let directions = case parseOnly inputFile file of
        Left err -> error err
        Right parsed -> parsed
  -- print directions
  let map = List.foldl' (\m ds -> Map.alter flipMU (List.foldl' move (Coord 0 0) ds) m) Map.empty directions
  print $ Map.size map
  let store = mkStore map
  let accessor = fst $ runStore $ iterate (extend rule) store !! 100
  print $ countTrue $ accessor <$> enumerate
  return ()

inputFile = line `sepBy1` endOfLine <* skipSpace <* endOfInput

line = many1 direction

direction = E <$ string "e" <|> SE <$ string "se" <|> SW <$ string "sw" <|> W <$ string "w" <|> NW <$ string "nw" <|> NE <$ string "ne"