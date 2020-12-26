module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import qualified Data.Vector.Mutable as Vector

input :: [Int]
-- input = [3, 8, 9, 1, 2, 5, 4, 6, 7]
input = [3, 6, 8, 1, 9, 5, 7, 4, 2]

moves, moves' :: Int
moves = 100
moves' = 10000000

copyInput xs mInput = do
  let zipped = zip xs (tail $ cycle xs)
  foldM_ (\mi (cup, nextCup) -> do Vector.write mi (cup -1) (nextCup -1); return mi) mInput zipped

go input moves = runST $ do
  vector <- Vector.new (length input)
  copyInput input vector
  foldM_ (\a _ -> play vector a) 2 [1 .. moves]
  getOutput vector

main :: IO ()
main = do
  let output = go input moves
  putStrLn $ foldMap show output
  let input' = input ++ [maximum input + 1 .. 1000000]
  let output = go input' moves'
  print $ product $ take 2 output
  return ()

getOutput mi = do
  let len = Vector.length mi
  res <- tail . reverse <$> foldM (\acc _ -> (: acc) <$> Vector.read mi (head acc)) [0] [1 .. len - 1]
  return $ (+ 1) <$> res

play input current = do
  next1 <- Vector.read input current
  next2 <- Vector.read input next1
  next3 <- Vector.read input next2
  next4 <- Vector.read input next3
  Vector.write input current next4
  let len = Vector.length input
  let possibleDestinations = filter (>= 0) [current -1, current -2, current -3, current - 4, len -1, len -2, len -3, len -4]
  let Just destination = find (`notElem` [next1, next2, next3]) possibleDestinations
  Vector.read input destination >>= Vector.write input next3
  Vector.write input destination next1
  return next4
