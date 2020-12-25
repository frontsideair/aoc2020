module Main (main) where

import Control.Arrow ((***))
import Control.Comonad
import Control.Comonad.Representable.Store (Store (..), StoreT (..), experiment, runStore, store)
import Data.Bool (bool)
import Data.Distributive (Distributive (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.List (find, foldl', length)
import qualified Data.Map.Strict as M
import qualified Data.Total.Map as T
import Text.Parsec hiding (Empty, count)
import Text.Parsec.String

size = 12

dbSize = size * 2 + 1

type Grid a = Store (T.TotalMap Coord) a

type Grid4 a = Store (T.TotalMap Coord4) a

data Coord = Coord Int Int Int deriving (Show, Eq, Ord)

instance Enum Coord where
  fromEnum (Coord x y z) = ((((x + size) * dbSize) + (y + size)) * dbSize) + (z + size)
  toEnum n = Coord (x - size) (y - size) (z - size)
    where
      (x, r) = n `quotRem` (dbSize * dbSize)
      (y, z) = r `quotRem` dbSize

instance Bounded Coord where
  minBound = Coord (- size) (- size) (- size)
  maxBound = Coord size size size

data Coord4 = Coord4 Int Int Int Int deriving (Show, Eq, Ord)

instance Enum Coord4 where
  fromEnum (Coord4 x y z t) = (((((x + size) * dbSize) + (y + size)) * dbSize) + (z + size)) * dbSize + (t + size)
  toEnum n = Coord4 (x - size) (y - size) (z - size) (t - size)
    where
      (x, r) = n `quotRem` (dbSize * dbSize * dbSize)
      (y, r') = r `quotRem` (dbSize * dbSize)
      (z, t) = r' `quotRem` dbSize

instance Bounded Coord4 where
  minBound = Coord4 (- size) (- size) (- size) (- size)
  maxBound = Coord4 size size size size

mkGrid :: [Coord] -> Grid Bool
mkGrid xs = store lookup $ Coord 0 0 0
  where
    lookup crd = crd `elem` xs

mkGrid4 :: [Coord4] -> Grid4 Bool
mkGrid4 xs = store lookup $ Coord4 0 0 0 0
  where
    lookup crd = crd `elem` xs

render :: Grid Bool -> String
render (StoreT (Identity (T.TotalMap map)) _) =
  foldMap ((++ "\n\n") . foldMap ((++ "\n") . foldMap (bool "." "#"))) $ (chunksOf dbSize . chunksOf dbSize) (snd <$> M.toList map)

chunksOf n [] = []
chunksOf n xs = let (l, r) = splitAt n xs in l : chunksOf n r

type Rule = Grid Bool -> Bool

type Rule4 = Grid4 Bool -> Bool

rule :: Rule
rule = ruleHelper neighborCoords

rule4 :: Rule4
rule4 = ruleHelper neighborCoords4

ruleHelper n g =
  if extract g
    then numActiveNeighbors `elem` [2, 3]
    else numActiveNeighbors == 3
  where
    activeNeighbors = experiment n g
    numActiveNeighbors = countTrue activeNeighbors

neighborCoords (Coord a b c) = [Coord x y z | x <- f a, y <- f b, z <- f c, (x, y, z) /= (a, b, c)]
  where
    f n = filter (within (- size, size)) [n -1, n, n + 1]

neighborCoords4 (Coord4 a b c d) = [Coord4 x y z t | x <- f a, y <- f b, z <- f c, t <- f d, (x, y, z, t) /= (a, b, c, d)]
  where
    f n = filter (within (- size, size)) [n -1, n, n + 1]

within (min, max) n = n >= min && n <= max

main :: IO ()
main = do
  Right input <- parseFromFile inputFile "./input"
  -- print input
  let pocket = mkGrid [Coord 0 b c | (b, row) <- input, (c, cell) <- row, cell == '#']
  -- putStrLn $ render pocket
  let accessor = fst $ runStore $ iterate (extend rule) pocket !! 6
  print $ countTrue $ accessor <$> [minBound .. maxBound]
  let pocket = mkGrid4 [Coord4 0 0 b c | (b, row) <- input, (c, cell) <- row, cell == '#']
  let accessor = fst $ runStore $ iterate (extend rule4) pocket !! 6
  print $ countTrue $ accessor <$> [minBound .. maxBound]
  return ()

countTrue = length . filter id

inputFile = zip [0 ..] <$> endBy1 row endOfLine <* eof

row = zip [0 ..] <$> many1 (char '#' <|> char '.')