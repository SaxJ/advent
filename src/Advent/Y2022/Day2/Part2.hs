{-# LANGUAGE OverloadedStrings #-}
module Advent.Y2022.Day2.Part2 where

import Advent.Input (readInput)
import Data.Text hiding (map)
import Prelude hiding (words, head)
import Data.Char (GeneralCategory(ClosePunctuation))


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
        -- x lose
        -- y draw
        -- z win
    x = case b of
          'X' -> 0
          'Y' -> 3
          'Z' -> 6
          _ -> 0
    y = case p of
          ('A', 'X') -> 3
          ('B', 'X') -> 1
          ('C', 'X') -> 2

          ('A', 'Y') -> 1
          ('B', 'Y') -> 2
          ('C', 'Y') -> 3

          ('A', 'Z') -> 2
          ('B', 'Z') -> 3
          ('C', 'Z') -> 1
          _ -> 0
