module Main (main) where

import Data.Char (isDigit, isHexDigit)
import Data.Either (fromRight)
import Data.Map.Strict (fromList, lookup, member)
import Text.Parsec
  ( char,
    digit,
    endOfLine,
    letter,
    space,
  )
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text (parseFromFile)
import Prelude hiding (lookup)

requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main :: IO ()
main = do
  parsed <- parseFromFile inputFile "./input"
  let passports = fromRight [] parsed
  let validPassports = filter isValid passports
  print $ length validPassports
  let validPassports = filter isValid2 passports
  print $ length validPassports
  return ()

isValid passport = all member' requiredKeys
  where
    member' key = member key passport

isValid2 passport = and [validByr, validIyr, validEyr, validHgt, validHcl, validEcl, validPid]
  where
    validByr = maybe False ((\year -> year >= 1920 && year <= 2002) <$> read) (lookup "byr" passport)
    validIyr = maybe False ((\year -> year >= 2010 && year <= 2020) <$> read) (lookup "iyr" passport)
    validEyr = maybe False ((\year -> year >= 2020 && year <= 2030) <$> read) (lookup "eyr" passport)
    validHgt = maybe False (checkUnit <$> span isDigit) (lookup "hgt" passport)
    validHcl = maybe False ((\(hash, code) -> hash == "#" && all isHexDigit code) <$> splitAt 1) (lookup "hcl" passport)
    validEcl = maybe False (\ecl -> ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) (lookup "ecl" passport)
    validPid = maybe False (\pid -> (length pid == 9) && all isDigit pid) (lookup "pid" passport)

checkUnit (hgt, "cm") = height >= 150 && height <= 193 where height = read hgt
checkUnit (hgt, "in") = height >= 59 && height <= 76 where height = read hgt
checkUnit _ = False

inputFile = do
  result <- passport `sepBy` endOfLine
  eof
  return result

passport = fromList <$> many keyval

keyval = do
  k <- key
  char ':'
  v <- val
  endOfLine <|> space
  return (k, v)

key = count 3 letter

val = many $ letter <|> digit <|> char '#'