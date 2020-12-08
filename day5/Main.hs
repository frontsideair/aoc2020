module Main (main) where

import Data.Char
import Data.Either (fromRight)
import Data.Functor
import Data.Set
import Numeric
import Text.Parsec
import Text.Parsec.Text

main :: IO ()
main = do
  parsed <- parseFromFile inputFile "./input"
  let seatNumbers = fromRight [] parsed
  let ids = fromList $ (\(row, col) -> row * 8 + col) <$> seatNumbers
  print $ findMax ids
  print $ difference (fromList [0 .. 1023]) ids
  return ()

inputFile = do
  result <- many seatNumber
  eof
  return result

seatNumber = do
  row <- count 7 (char 'B' $> '1' <|> char 'F' $> '0')
  column <- count 3 (char 'R' $> '1' <|> char 'L' $> '0')
  endOfLine
  return (readBin row, readBin column)

readBin num = fst $ head $ readInt 2 (`elem` "01") digitToInt num