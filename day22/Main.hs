{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Text
  ( Parser,
    digit,
    endOfInput,
    endOfLine,
    many1,
    parseOnly,
    sepBy1,
    skipSpace,
    string,
  )
import Data.Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text.IO as Text
import Debug.Trace

data Winner = Player1 | Player2 deriving (Show, Eq)

main :: IO ()
main = do
  file <- Text.readFile "./input"
  let (cards1, cards2) = case parseOnly parser file of
        Left err -> error err
        Right parsed -> parsed
  print $ calculateScore $ play cards1 cards2
  print $ calculateScore $ snd $ play2 Set.empty cards1 cards2
  return ()

calculateScore = sum . zipWith (*) [1 ..] . reverse

play [] cards2 = cards2
play cards1 [] = cards1
play (card1 : cards1) (card2 : cards2) = case compare card1 card2 of
  LT -> play cards1 (cards2 ++ [card2, card1])
  GT -> play (cards1 ++ [card1, card2]) cards2

play2 history [] cards2 = (Player2, cards2)
play2 history cards1 [] = (Player1, cards1)
play2 history (card1 : cards1) (card2 : cards2)
  | Set.member gameState history = (Player1, cards1)
  | card1 <= length cards1 && card2 <= length cards2 =
    case play2 Set.empty (take card1 cards1) (take card2 cards2) of
      (Player1, _) -> player1win
      (Player2, _) -> player2win
  | otherwise =
    case compare card1 card2 of
      LT -> player2win
      GT -> player1win
  where
    player1win = play2 history' (cards1 ++ [card1, card2]) cards2
    player2win = play2 history' cards1 (cards2 ++ [card2, card1])
    history' = Set.insert gameState history
    gameState = (card1 : cards1, card2 : cards2)

parser = do
  string "Player 1:"
  skipSpace
  cards1 <- sepBy1 integer endOfLine
  skipSpace
  string "Player 2:"
  skipSpace
  cards2 <- sepBy1 integer endOfLine
  skipSpace
  endOfInput
  return (cards1, cards2)

integer :: Parser Int
integer = read <$> many1 digit