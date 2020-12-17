module Main (main) where

import Data.List
import Text.Parsec
import Text.Parsec.String

withinRange (lower, upper) n = n >= lower && n <= upper

isValid n (_, range1, range2) = withinRange range1 n || withinRange range2 n

isValidTicket rules = all (\n -> any (isValid n) rules)

startsWith list sublist = take (length sublist) list == sublist

main :: IO ()
main = do
  Right (rules, myTicket, nearbyTickets) <- parseFromFile inputFile "./input"
  -- print (rules, myTicket, nearbyTickets)
  print $ sum $ filter (\n -> all (not . isValid n) rules) $ concat nearbyTickets
  let validTickets = filter (isValidTicket rules) nearbyTickets
  -- print $ filter (\rules -> and $ and . zipWith (flip isValid) rules <$> validTickets) (permutations rules)
  -- print $ foldl' (\perms ticket -> filter (and . zipWith isValid ticket) perms) (permutations rules) validTickets
  let ticketsByFields = transpose validTickets
  let f rules field = filter (\rule -> all (`isValid` rule) field) rules
  let sortedRules = sortOn (length . snd) $ zip [0 ..] $ f rules <$> ticketsByFields
  let x = foldl' (\acc (index, rules) -> let r = (rules \\ (snd <$> acc)) in (index, head r) : acc) [] sortedRules
  let departures = fst <$> filter (\(_, (fieldName, _, _)) -> fieldName `startsWith` "departure") x
  print $ product $ (myTicket !!) <$> departures
  return ()

inputFile = do
  rules <- endBy1 (try rule) endOfLine
  endOfLine
  string "your ticket:"
  endOfLine
  myTicket <- ticket
  count 2 endOfLine
  string "nearby tickets:"
  endOfLine
  nearbyTickets <- endBy1 ticket endOfLine
  eof
  return (rules, myTicket, nearbyTickets)

rule = do
  fieldName <- many1 (letter <|> space)
  string ": "
  range1 <- range
  string " or "
  range2 <- range
  return (fieldName, range1, range2)

range :: Parser (Int, Int)
range = do
  lower <- number
  char '-'
  upper <- number
  return (lower, upper)

ticket :: Parser [Int]
ticket = sepBy1 number (char ',')

number = read <$> many1 digit