{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Either
import Data.List (find)
import Data.Set
import Text.Parsec
import Text.Parsec.Text

data Instruction = Nop Int | Acc Int | Jmp Int deriving (Show)

data Machine = Machine {index :: Int, accumulator :: Int, program :: [Instruction], visited :: Set Int} deriving (Show)

evolve [] = []
evolve ((Acc n) : program') = (:) (Acc n) <$> evolve program'
evolve ((Nop n) : program') = (Jmp n : program') : ((:) (Nop n) <$> evolve program')
evolve ((Jmp n) : program') = (Nop n : program') : ((:) (Jmp n) <$> evolve program')

run machine@Machine {index, accumulator, program, visited} = if member index visited then machine else run machine'
  where
    currentInstruction = program !! index
    machine' = case currentInstruction of
      Nop _ -> updateMachine 1 0
      Acc n -> updateMachine 1 n
      Jmp n -> updateMachine n 0
    updateMachine offset acc = Machine (index + offset) (accumulator + acc) program (insert index visited)

main :: IO ()
main = do
  parsed <- parseFromFile inputFile "./input"
  let program = fromRight [] parsed
  print $ accumulator $ run (Machine 0 0 program empty)
  let evolutions = evolve program
  print $ accumulator <$> find (\machine -> index machine == length program) ((\p -> run (Machine 0 0 p (singleton $ length p))) <$> evolutions)
  return ()

inputFile = do
  result <- many line
  eof
  return result

line = do
  instruction <- Nop <$> inst "nop" <|> Acc <$> inst "acc" <|> Jmp <$> inst "jmp"
  endOfLine
  return instruction

inst name = do
  string name
  space
  sign <- char '+' <|> char '-'
  n <- many1 digit
  return (read n * if sign == '-' then -1 else 1)
