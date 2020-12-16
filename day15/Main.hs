module Main (main) where

import Data.Map.Strict
import qualified Data.Map.Strict as M

type History = Map Int Int

initialHistory = zip input [1 ..]

-- input = [0, 3, 6]

input = [7, 14, 0, 17, 11, 1, 2]

game target history (lastNumber, index) =
  if target == index
    then lastNumber
    else case age of
      Just age -> recur (index - age)
      Nothing -> recur 0
  where
    age = history !? lastNumber
    recur n = game target (insert lastNumber index history) (n, index + 1)

main :: IO ()
main = do
  print $ game 2020 (fromList $ init initialHistory) (last initialHistory)
  print $ game 30000000 (fromList $ init initialHistory) (last initialHistory)
  return ()