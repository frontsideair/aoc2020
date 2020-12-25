module Main (main) where

year = 2020

main :: IO ()
main = do
  file <- readFile "./input"
  let numbers = read <$> lines file :: [Int]
  let results = [a * b | a <- numbers, b <- numbers, a + b == year]
  print results
  let results = [a * b * c | a <- numbers, b <- numbers, c <- numbers, a + b + c == year]
  print results
  return ()