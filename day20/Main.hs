{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Attoparsec.ByteString.Char8 hiding (Done, take, takeWhile)
import qualified Data.ByteString as BS
import Data.List (foldl1, maximum, union)
import qualified Data.Map as Map
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified GHC.Show
import Relude
import qualified Relude.Unsafe as Unsafe
import System.IO (hPutStrLn)

newtype MyBool = MyBool Bool deriving (Eq, Ord)

instance Show MyBool where
  show (MyBool True) = "#"
  show (MyBool False) = "."

data Tile = Tile {tileId :: TileId, unTile :: Matrix MyBool}
  deriving (Show, Eq)

newtype TileId = TileId {unTileId :: Int}
  deriving (Show, Eq, Ord)

newtype Edge = Edge {unEdge :: Vector MyBool}
  deriving (Show, Eq, Ord)

type Input = [Tile]

data Direction = DTop | DBottom | DLeft | DRight deriving (Show, Eq, Ord)

seaMonster =
  Matrix.fromList
    3
    20
    "                  # \
    \#    ##    ##    ###\
    \ #  #  #  #  #  #   "

checkSeaMonster matrix (offsetX, offsetY) =
  all check [(i, j) | i <- [1 .. Matrix.nrows seaMonster], j <- [1 .. Matrix.ncols seaMonster]]
  where
    check (i, j) = seaMonster Matrix.! (i, j) == ' ' || Matrix.safeGet (i + offsetX - 1) (j + offsetY - 1) matrix == Just (MyBool True)

checkSeaMonsters matrix =
  length $ filter identity $ checkSeaMonster matrix <$> [(i, j) | i <- [1 .. Matrix.nrows matrix], j <- [1 .. Matrix.ncols matrix]]

parser :: Parser Input
parser = flip sepBy endOfLine $ do
  tid <- TileId <$> (string "Tile " *> decimal <* char ':' <* endOfLine)
  Tile tid <$> tile
  where
    cell =
      (char '.' $> MyBool False)
        <|> (char '#' $> MyBool True)
    tile = Matrix.fromLists <$> many1 (many1 cell <* endOfLine)

rotations :: Tile -> [Tile]
rotations (Tile i m) = map (Tile i) (rotationsM m)

rotationsM :: Matrix a -> [Matrix a]
rotationsM t =
  let xs = take 4 (iterate rotateLeft t)
   in xs ++ map flipMatrix xs

rotateLeft = Matrix.transpose . flipMatrix

flipMatrix t = Matrix.mapPos (\(x, y) _ -> t Matrix.! (x, Matrix.ncols t - y + 1)) t

leftEdge, rightEdge, topEdge, bottomEdge :: Tile -> Edge
topEdge = Edge . Matrix.getRow 1 . unTile
bottomEdge (Tile _ t) = Edge $ Matrix.getRow (Matrix.nrows t) t
leftEdge = Edge . Matrix.getCol 1 . unTile
rightEdge (Tile _ t) = Edge $ Matrix.getCol (Matrix.ncols t) t

flipEdge :: Edge -> Edge
flipEdge = Edge . V.reverse . unEdge

tileEdges :: Tile -> [(Edge, Direction)]
tileEdges t = [(leftEdge t, DLeft), (rightEdge t, DRight), (topEdge t, DTop), (bottomEdge t, DBottom)]

allTileEdges t = xs ++ (flipEdge <$> xs)
  where
    xs = fst <$> tileEdges t

run1 :: Input -> _
run1 input =
  let edges =
        input
          & map (\t -> Map.fromList $ map (,[tileId t]) (allTileEdges t))
          & foldl' (Map.unionWith (++)) Map.empty
   in edges
        & Map.elems
        & mapMaybe
          ( \case
              [t1, t2] -> Just (t1, t2)
              [_] -> Nothing
              _ -> error "foo"
          )
        & ordNub
        & concatMap (\(i, j) -> [i, j])
        & sort
        & group
        & map (\xs@(x : _) -> (x, length xs))
        & filter ((== 2) . snd)
        & map fst
        & map unTileId
        & product

run2 :: Input -> _
run2 input =
  let initTile : unknownTiles = input
      initPos = (0, 0)
      (edges :: Map (Edge, Direction) [Tile]) =
        input
          & concatMap rotations
          & map (\t -> Map.fromList $ map (,[t]) (tileEdges t))
          & foldl' (Map.unionWith (++)) Map.empty
      tiles =
        input
          & map tileId
          & Set.fromList
      solution = solve edges [(initPos, initTile)] Map.empty tiles
      matrixSize = floor $ sqrt $ fromIntegral $ Map.size solution
      assembled = assemble matrixSize solution
      numMonsters = maximum $ checkSeaMonsters <$> rotationsM assembled
      monsterTiles = length $ filter (== '#') $ Matrix.toList seaMonster
      mapTiles = length $ filter (== MyBool True) $ Matrix.toList assembled
   in mapTiles - (numMonsters * monsterTiles)

solve _ [] placed _ = placed
solve edges (((y, x), tile) : queue) placed unplaced
  | Set.null unplaced = placed
  | otherwise =
    solve
      edges
      (queue ++ neighbors)
      (Map.insert (y, x) tile placed)
      (Set.delete (tileId tile) unplaced)
  where
    neighbors =
      mapMaybe
        lookup
        [ ((y - 1, x), (topEdge tile, DBottom)),
          ((y + 1, x), (bottomEdge tile, DTop)),
          ((y, x + 1), (rightEdge tile, DLeft)),
          ((y, x - 1), (leftEdge tile, DRight))
        ]
    lookup (coord, edge) = do
      candidate <- find (\t -> tileId t /= tileId tile) $ edges Map.! edge
      if Set.member (tileId tile) unplaced then Just (coord, candidate) else Nothing

dropEdges matrix = Matrix.submatrix 2 (Matrix.nrows matrix - 1) 2 (Matrix.ncols matrix - 1) matrix

assemble n solution = Matrix.flatten $ Matrix.matrix n n index
  where
    map = dropEdges . unTile <$> solution
    ((iMin, jMin), _) = Map.findMin map
    index (i, j) = map Map.! (i + iMin - 1, j + jMin - 1)

main :: IO ()
main = do
  bs <- BS.readFile "./input"
  parsed <-
    case parseOnly (parser <* endOfInput) bs of
      Right r -> return r
      Left err -> do
        hPutStrLn stderr $ "parse failed: " <> show err
        exitFailure
  print $ run1 parsed
  print $ run2 parsed
