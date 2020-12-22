{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Attoparsec.Text hiding (number)
import Data.Function
import Data.Functor
import Data.List (elem, (\\))
import Data.Map hiding (singleton, (\\))
import Data.Text hiding (length)
import Data.Text.IO
import Prelude hiding (readFile)

main :: IO ()
main =
  do
    -- p <- parseFromFile inputFile "./input.test"
    -- print p
    file <- readFile "./input"
    let (rules, messages) = case parseOnly inputFile file of
          Left err -> error err
          Right parsed -> parsed
    let parser = makeParser rules (rules ! 0) <* endOfInput
    let results = [result | Right result <- parseOnly parser <$> messages]
    print $ length results
    let parser' = part2 rules <* endOfInput
    let results' = [result | Right result <- parseOnly parser' <$> messages]
    print $ length results'
    -- traverse print $ results' \\ results
    -- let valids = ["bbabbbbaabaabba", "babbbbaabbbbbabbbbbbaabaaabaaa", "aaabbbbbbaaaabaababaabababbabaaabbababababaaa", "bbbbbbbaaaabbbbaaabbabaaa", "bbbababbbbaaaaaaaabbababaaababaabab", "ababaaaaaabaaab", "ababaaaaabbbaba", "baabbaaaabbaaaababbaababb", "abbbbabbbbaaaababbbbbbaaaababb", "aaaaabbaabaaaaababaa", "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"]
    -- traverse print $ parseOnly (parser' rules) <$> valids
    -- let r = fromList [(0, Expression (Sum (Reference 1) (Product (Reference 1) (Reference 0)))), (1, Literal "a")]
    -- print $ parseOnly (makeParser r (r ! 0)) "aa"
    -- print $ parseOnly (customRule) "aa"
    return ()

part2 rules = do
  a <- many1 $ makeParser rules (rules ! 42)
  b <- many1 $ makeParser rules (rules ! 31)
  if length a > length b then return () else fail "nope"

customRule = ((<>) <$> string "a" <*> customRule) <|> string "a"

rule8 = Expression [[42], [42, 8]]

rule11 = Expression [[42, 31], [42, 11, 31]]

makeParser _ (Literal c) = string c
makeParser m (Expression e) = expression m e

expression m [p] = prod m p
expression m (p : rest) = prod m p <|> expression m rest

prod m [i] = makeParser m (m ! i)
prod m (i : rest) = (<>) <$> makeParser m (m ! i) <*> prod m rest

-- makeParser' m (Product a b) = (<>) <$> makeParser' m a <*> makeParser' m b
-- makeParser' m (Sum a b) = makeParser' m a <|> makeParser' m b

data Rule = Literal Text | Expression [[Int]] deriving (Show)

rule = do
  index <- number
  string ": "
  r <- expr <|> literal
  return (index, r)

expr = Expression <$> term `sepBy1` string " | "

term = number `sepBy1` char ' '

literal = do
  char '"'
  c <- string "a" <|> string "b"
  char '"'
  return $ Literal c

final p = do
  r <- p
  endOfInput
  return r

-- parser = final rule0 <|> final rule1 <|> final rule2 <|> final rule3 <|> final rule4 <|> final rule5

-- rule0 = try $ rule4 >> rule1 >> rule5

-- rule1 = try $ (rule2 >> rule3) <|> (rule3 >> rule2)

-- rule2 = try $ (rule4 >> rule4) <|> (rule5 >> rule5)

-- rule3 = try $ (rule4 >> rule5) <|> (rule5 >> rule4)

-- rule4 = char 'a' $> ()

-- rule5 = char 'b' $> ()

inputFile = do
  rules <- sepBy1 rule endOfLine
  skipSpace
  messages <- sepBy1 message endOfLine
  skipSpace
  endOfInput
  return (fromList rules, messages)

message :: Parser Text
message = pack <$> many1 letter

number = read <$> many1 digit

-- endBy1 p sep = sepBy1 p sep <* sep

-- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do a <- p; rest a
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        <|> return a
