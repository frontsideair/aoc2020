{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Arrow ((***))
import Control.Comonad
import Control.Comonad.Representable.Store (Store (..), StoreT (..), experiment, runStore, store)
import Data.Bool (bool)
import Data.Distributive (Distributive (..))
import Data.Either
import Data.Functor
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.List (find)
import Data.Maybe
import Data.Vector (Vector, fromList, generate, (!), (!?))
import Text.Parsec hiding (Empty, count)
import Text.Parsec.Text

data Position = Floor | Empty | Occupied deriving (Eq)

instance Show Position where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

newtype VBounded a = VBounded (Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

-- (filename, width, height) = ("./input.test", 10, 10)

(filename, width, height) = ("./input", 91, 98)

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v ! i
  tabulate desc = VBounded $ generate (max width height) desc

type Grid a = Store (Compose VBounded VBounded) a

type Coord = (Int, Int)

mkGrid xs = store lookup (0, 0)
  where
    lookup (i, j) = fromMaybe Floor $ (xs !? i) >>= (!? j)

type Rule = Grid Position -> Position

rule :: Rule
rule g = case extract g of
  Floor -> Floor
  Empty -> if occupiedNeighbors == 0 then Occupied else Empty
  Occupied -> if occupiedNeighbors >= 4 then Empty else Occupied
  where
    neighborCoords (a, b) = [(x, y) | x <- [a -1, a, a + 1], y <- [b -1, b, b + 1], (x, y) /= (a, b), x `within` height, y `within` width]
    neighbors = experiment neighborCoords g
    occupiedNeighbors = count (== Occupied) neighbors
    a `within` range = a >= 0 && a < range

rule2 :: Rule
rule2 g = case extract g of
  Floor -> Floor
  Empty -> if occupiedNeighbors == 0 then Occupied else Empty
  Occupied -> if occupiedNeighbors >= 5 then Empty else Occupied
  where
    neighbors = [find (/= Floor) $ experiment (\(a, b) -> [(x, y) | i <- [1, 2 .. (max width height)], let (x, y) = direction (a, b) i, x `within` height, y `within` width]) g | direction <- [up, down, left, right, upright, upleft, downright, downleft]]
    occupiedNeighbors = count (== Occupied) (catMaybes neighbors)
    a `within` range = a >= 0 && a < range
    up (x, y) i = (x - i, y)
    down (x, y) i = (x + i, y)
    left (x, y) i = (x, y - i)
    right (x, y) i = (x, y + i)
    upleft (x, y) i = (x - i, y - i)
    upright (x, y) i = (x - i, y + i)
    downleft (x, y) i = (x + i, y - i)
    downright (x, y) i = (x + i, y + i)

step :: Rule -> Grid Position -> Grid Position
step = extend

render :: Grid Position -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap show) g

loop stepper game = if render game' == render game then game else loop stepper game'
  where
    game' = stepper game

count pred xs = length $ filter pred xs

main :: IO ()
main = do
  parsed <- parseFromFile inputFile filename
  let grid = mkGrid $ fromList $ fromList <$> fromRight [] parsed
  -- putStrLn $ render grid
  -- putStrLn $ render $ loop (step rule) grid
  let (endGame, _) = runStore $ loop (step rule) grid
  print $ count (== Occupied) $ endGame <$> [(a, b) | a <- [0 .. height - 1], b <- [0 .. width - 1]]
  putStrLn $ render $ step rule2 grid
  putStrLn $ render $ step rule2 $ step rule2 grid
  putStrLn $ render $ step rule2 $ step rule2 $ step rule2 grid
  let (endGame, _) = runStore $ loop (step rule2) grid
  print $ count (== Occupied) $ endGame <$> [(a, b) | a <- [0 .. height - 1], b <- [0 .. width - 1]]
  return ()

inputFile = endBy line endOfLine <* eof

line = many1 ((char '.' $> Floor) <|> (char 'L' $> Empty) <|> (char '#' $> Occupied))
