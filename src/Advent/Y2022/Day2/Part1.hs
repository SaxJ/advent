{-# LANGUAGE OverloadedStrings #-}
module Advent.Y2022.Day2.Part1 where

import Advent.Input (readInput)
import Data.Text hiding (map)
import GHC.Float (expts10)
import Prelude hiding (words, head)
import Data.Char (GeneralCategory(ClosePunctuation))

-- x lose
-- y draw
-- z win

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2022/Day2/input"
  print $ solve lines

solve :: [Text] -> Int
solve lines = sum $ map pairToScore pairs
  where
    pairs = map lineToPair lines

lineToPair :: Text -> (Char, Char)
lineToPair l = (x, y)
  where
    [xs, ys] = words l
    x = head xs
    y = head ys

pairToScore :: (Char, Char) -> Int
pairToScore p@(a, b) = x + y
  where
    x = case b of
          'X' -> 1
          'Y' -> 2
          'Z' -> 3
          _ -> 0
    y = case p of
          ('A', 'X') -> 3
          ('B', 'Y') -> 3
          ('C', 'Z') -> 3

          ('A', 'Y') -> 6
          ('B', 'Z') -> 6
          ('C', 'X') -> 6

          ('A', 'Z') -> 0
          ('B', 'X') -> 0
          ('C', 'Y') -> 0
          _ -> 0
