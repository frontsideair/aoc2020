{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Either
import Data.List
import Text.Parsec hiding (State)
import Text.Parsec.String
import Prelude hiding (cos, mod, sin)

data Action = N Int | S Int | E Int | W Int | L Int | R Int | F Int deriving (Show)

data Coordinates = Coordinates {horizontal :: Int, vertical :: Int} deriving (Show)

data FerryState = FerryState {coordinates :: Coordinates, rotation :: Int} deriving (Show)

act 'N' = N
act 'S' = S
act 'E' = E
act 'W' = W
act 'L' = L
act 'R' = R
act 'F' = F

initialState = FerryState (Coordinates 0 0) 0

updateVertical n coords = coords {vertical = vertical coords + n}

updateHorizontal n coords = coords {horizontal = horizontal coords + n}

step state (N n) = state {coordinates = updateVertical n (coordinates state)}
step state (S n) = state {coordinates = updateVertical (- n) (coordinates state)}
step state (E n) = state {coordinates = updateHorizontal n (coordinates state)}
step state (W n) = state {coordinates = updateHorizontal (- n) (coordinates state)}
step state (L n) = state {rotation = (rotation state + n) `mod` 360}
step state (R n) = state {rotation = (rotation state - n) `mod` 360}
step state (F n) = state {coordinates = updateHorizontal x (updateVertical y (coordinates state))}
  where
    x = cos (rotation state) * n
    y = sin (rotation state) * n

sin 0 = 0
sin 90 = 1
sin 180 = 0
sin 270 = -1

cos 0 = 1
cos 90 = 0
cos 180 = -1
cos 270 = 0

a `mod` b
  | a < 0 = (a + b) `mod` b
  | a >= b = (a - b) `mod` b
  | otherwise = a

-- a `mod'` b = if rem < 0 then rem + b else rem where rem = a `mod` b

manhattanDistance coords = abs (vertical coords) + abs (horizontal coords)

data State = State {waypoint :: Coordinates, ferry :: Coordinates}

step2 state (N n) = state {waypoint = updateVertical n (waypoint state)}
step2 state (S n) = state {waypoint = updateVertical (- n) (waypoint state)}
step2 state (E n) = state {waypoint = updateHorizontal n (waypoint state)}
step2 state (W n) = state {waypoint = updateHorizontal (- n) (waypoint state)}
step2 state (L n) = state {waypoint = rotate n $ waypoint state}
step2 state (R n) = state {waypoint = rotate ((- n) `mod` 360) $ waypoint state}
step2 state (F n) = state {ferry = updateHorizontal x (updateVertical y (ferry state))}
  where
    x = n * horizontal (waypoint state)
    y = n * vertical (waypoint state)

rotate 0 coords = coords
rotate 90 Coordinates {horizontal, vertical} = Coordinates {horizontal = - vertical, vertical = horizontal}
rotate 180 Coordinates {horizontal, vertical} = Coordinates {horizontal = - horizontal, vertical = - vertical}
rotate 270 Coordinates {horizontal, vertical} = Coordinates {horizontal = vertical, vertical = - horizontal}

initialState2 = State {waypoint = Coordinates {vertical = 1, horizontal = 10}, ferry = Coordinates 0 0}

main :: IO ()
main = do
  parsed <- parseFromFile inputFile "./input"
  let actions = fromRight [] parsed
  -- print $ foldl' step initialState <$> inits actions
  print $ manhattanDistance $ coordinates $ foldl' step initialState actions
  print $ manhattanDistance $ ferry $ foldl' step2 initialState2 actions
  return ()

inputFile :: Stream s m Char => ParsecT s u m [Action]
inputFile = endBy line endOfLine <* eof

line :: Stream s m Char => ParsecT s u m Action
line = action 'N' <|> action 'S' <|> action 'E' <|> action 'W' <|> action 'L' <|> action 'R' <|> action 'F'

action :: Stream s m Char => Char -> ParsecT s u m Action
action c = act c . read <$> (char c *> many1 digit)