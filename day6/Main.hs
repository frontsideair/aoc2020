module Main (main) where

import Data.Either
import Data.Set
import Text.Parsec
import Text.Parsec.Text

main :: IO ()
main = do
  parsed <- parseFromFile inputFile "./input"
  -- print parsed
  let groups = fromRight [] parsed
  print $ sum $ size . foldl1 union <$> groups
  print $ sum $ size . foldl1 intersection <$> groups
  return ()

inputFile = do
  result <- group `sepBy` endOfLine
  eof
  return result

group = many line

line = do
  result <- many1 letter
  endOfLine
  return $ fromList result