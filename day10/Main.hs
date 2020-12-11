module Main (main) where

import Control.Monad
import Data.List

main :: IO ()
main = do
  input <- fmap read . lines <$> readFile "./input"
  let sorted = sort (0 : input)
  let differences = 3 : zipWith (-) (tail sorted) sorted
  print $ length (filter (== 1) differences) * length (filter (== 3) differences)
  let contiguous = length <$> filter ((/= 3) . head) (group differences)
  let x = (\n -> nub $ filter ((== n) . sum) (replicateM n [1, 2, 3] >>= inits)) <$> contiguous
  print $ product (length <$> x)
  return ()