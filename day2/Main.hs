{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Bits (Bits (xor))
import Data.Either (fromRight)
import Data.String (IsString (fromString))
import Data.Text (Text, count, index, singleton)
import Text.Parsec
  ( char,
    digit,
    endOfLine,
    eof,
    letter,
    many,
    space,
    string,
  )
import Text.Parsec.Text (parseFromFile)
import Prelude hiding (max, min)

main :: IO ()
main = do
  passwordLines <- fromRight [] <$> parseFromFile inputFile "./input"
  let validPasswords = filter meetsRule passwordLines
  print $ length validPasswords
  let validPasswords = filter meetsRule2 passwordLines
  print $ length validPasswords

meetsRule :: (Rule, Text) -> Bool
meetsRule (Rule {min, max, character}, password) =
  min <= freq && freq <= max
  where
    freq = count (singleton character) password

meetsRule2 (Rule {min, max, character}, password) =
  xor (minChar == character) (maxChar == character)
  where
    minChar = index password (min - 1)
    maxChar = index password (max - 1)

data Rule = Rule
  {min :: Int, max :: Int, character :: Char}
  deriving (Show)

inputFile = do
  result <- many line
  eof
  return result

line = do
  min <- many digit
  char '-'
  max <- many digit
  space
  character <- letter
  string ": "
  password <- many letter
  endOfLine
  return (Rule (read min) (read max) character, fromString password)
