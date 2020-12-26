module Main (main) where

import qualified Data.List as List

cardPubKey, doorPubKey, subjectNumber, initialNumber, divisor :: Integer
-- cardPubKey = 5764801
cardPubKey = 5290733
-- doorPubKey = 17807724
doorPubKey = 15231938
subjectNumber = 7
initialNumber = 1
divisor = 20201227

step subjectNumber n = (n * subjectNumber) `rem` divisor

loop 0 f x = f x
loop n f x = loop (n -1) f (f x)

main :: IO ()
main = do
  let Just (cardLoopSize, _) = List.find ((== cardPubKey) . snd) $ zip [0 ..] $ iterate (step subjectNumber) initialNumber
  let Just (doorLoopSize, _) = List.find ((== doorPubKey) . snd) $ zip [0 ..] $ iterate (step subjectNumber) initialNumber
  print (cardPubKey, doorLoopSize)
  print (doorPubKey, cardLoopSize)
  let (pubKey, loopSize) = if doorLoopSize < cardLoopSize then (cardPubKey, doorLoopSize) else (doorPubKey, cardLoopSize)
  print $ iterate (step pubKey) initialNumber !! loopSize
  return ()