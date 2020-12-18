{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Data.Functor
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  Right expressions <- parseFromFile (inputFile expression) "./input"
  print $ sum expressions
  Right expressions <- parseFromFile (inputFile expression2) "./input"
  print $ sum expressions
  return ()

inputFile exp = endBy1 exp endOfLine <* eof

expression = chainl1 term (addition <|> multiplication)

parens = between (char '(') (char ')')

term = parens expression <|> number

number = read <$> many1 digit

betweenSpaces = between (char ' ') (char ' ')

addition = try $ betweenSpaces $ try $ char '+' $> (+)

multiplication = betweenSpaces $ char '*' $> (*)

expression2 = chainl1 term2 multiplication

term2 = chainl1 factor2 addition

factor2 = parens expression2 <|> number