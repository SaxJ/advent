module Advent.Y2022.Day1.Part1 where

import Advent.Input
import Data.Text hiding (map, maximum)
import Data.List.Split

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2022/Day1/input"
  let ans = solve lines
  print ans

groupSum :: [Text] -> Int
groupSum lines = sum $ map (read . unpack) lines

solve :: [Text] -> Int
solve lines = maximum $ map groupSum groups
  where
    groups = splitWhen (== "") lines
