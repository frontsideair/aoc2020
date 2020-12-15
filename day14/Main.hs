module Main (main) where

import Control.Applicative hiding (empty, (<|>))
import Control.Arrow
import Control.Monad
import Data.Bits
import Data.List (elemIndices, foldl')
import Data.Map.Strict hiding (foldl')
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String
import Prelude hiding (replicate, sum)

type Mask = String

data Instruction = UpdateMask Mask | WriteToMemory Int Int deriving (Show)

type Memory = (Mask, Map Int Int)

initialMemory = ("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", empty)

interpret :: Instruction -> Memory -> Memory
interpret (UpdateMask mask) (_, memory) = (mask, memory)
interpret (WriteToMemory address value) (mask, memory) = (mask, insert address (maskValue mask value) memory)

maskValue mask value = foldl' clearBit (foldl' setBit value ones) zeroes
  where
    ones = elemIndices '1' reversed
    zeroes = elemIndices '0' reversed
    reversed = reverse mask

interpret2 :: Instruction -> Memory -> Memory
interpret2 (UpdateMask mask) (_, memory) = (mask, memory)
interpret2 (WriteToMemory address value) (mask, memory) = (mask, bulkInsert (maskAddress mask address) value memory)

maskAddress :: String -> Int -> [Int]
maskAddress mask address = foldM (\b a -> [setBit b a, clearBit b a]) overwritten xes
  where
    overwritten = foldl' setBit address ones
    xes = elemIndices 'X' reversed
    ones = elemIndices '1' reversed
    reversed = reverse mask

bulkInsert :: [Int] -> a -> Map Int a -> Map Int a
bulkInsert keys value map = foldl' (\map key -> insert key value map) map keys

mapSum = M.foldl' (+) 0

main :: IO ()
main = do
  Right program <- parseFromFile inputFile "./input"
  -- print program
  print $ mapSum . snd $ foldl' (flip interpret) initialMemory program
  print $ mapSum . snd $ foldl' (flip interpret2) initialMemory program
  return ()

inputFile = endBy1 (updateMask <|> writeToMemory) endOfLine <* eof

updateMask = do
  try $ string "mask = "
  UpdateMask <$> count 36 (char 'X' <|> char '1' <|> char '0')

writeToMemory = do
  string "mem["
  address <- many1 digit
  string "] = "
  value <- many1 digit
  return $ WriteToMemory (read address) (read value)