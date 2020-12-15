module Main (main) where

import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  parsed <- parseFromFile inputFile "./input"
  -- print parsed
  let Right (arrivalTime, chart) = parsed
  let buses = catMaybes chart
  let busTimelines = catMaybes <$> transpose ((\bus -> (\time -> if time `mod` bus == 0 then Just bus else Nothing) <$> [0 ..]) <$> buses)
  let Just (time, [bus]) = find ((== 1) . length . snd) $ drop arrivalTime $ zip [0 ..] busTimelines
  print $ bus * (time - arrivalTime)
  let modsAndRems = sortOn (Down . fst) $ catMaybes $ zipWith (\a b -> f <$> a <*> Just b) chart [0 ..]
  print modsAndRems
  print $ chineseRemainder modsAndRems
  return ()

f a b = (a, (a - b) `mod` a)

chineseRemainder ((m, r) : rest) = go m r rest
  where
    go _ acc [] = acc
    go increment acc ((m, r) : rest) = if acc `mod` m == r then go (m * increment) acc rest else go increment (acc + increment) ((m, r) : rest)

inputFile :: Parser (Int, [Maybe Int])
inputFile = do
  arrivalTime <- many1 digit <* endOfLine
  buses <- sepBy1 (many1 (digit <|> char 'x')) (char ',') <* endOfLine
  eof
  return (read arrivalTime, (\bus -> if bus == "x" then Nothing else Just $ read bus) <$> buses)