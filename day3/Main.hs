module Main (main) where

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  file <- readFile "./input"
  let map = cycle <$> lines file
  let trees (right, down) = length $ filter (== '#') $ slope map (right, down)
  print $ trees (3, 1)
  print $ product $ trees <$> slopes

slope map (right, down) = zipWith (!!) map' indices
  where
    map' = [x | (x, index) <- zip map [0 ..], index `mod` down == 0]
    indices = [0, right ..]
