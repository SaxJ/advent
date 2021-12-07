module Advent.Y2020.Day10.Part1 where

import Advent.Input
import Data.List (maximum, sort)

differences :: [Integer] -> [Integer]
differences xs = case xs of
  (x : y : z) -> y - x : differences (y : z)
  [_] -> []
  _ -> []

solve :: [Integer] -> Int
solve input = length ones * length threes
  where
    gaps = differences $ sort (0 : input ++ [maximum input + 3])
    ones = filter (== 1) gaps
    threes = filter (== 3) gaps

solution :: IO ()
solution = do
  input <- readIntegers "src/Advent/Y2020/Day10/input"
  print $ solve input
