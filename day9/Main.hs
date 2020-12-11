{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Arrow

preambleSize = 25

inputFile = "./input"

main :: IO ()
main = do
  file :: [Int] <- fmap read . lines <$> readFile inputFile
  let Just invalid = test file
  print invalid
  let (a, b) = (minimum &&& maximum) (takeWhileSum file invalid)
  print $ a + b
  return ()

test xs
  | length xs <= preambleSize = Nothing
  | otherwise = if found then test $ tail xs else Just number
  where
    (preamble, rest) = splitAt preambleSize xs
    number = head rest
    found = number `elem` [a + b | a <- preamble, b <- preamble, a /= b]

takeWhileSum (y : ys) sum = takeWhileSum' [] 0 (y : ys)
  where
    takeWhileSum' _ _ [] = []
    takeWhileSum' acc accSum (x : xs) = case (accSum + x) `compare` sum of
      LT -> takeWhileSum' (x : acc) (accSum + x) xs
      EQ -> x : acc
      GT -> takeWhileSum ys sum