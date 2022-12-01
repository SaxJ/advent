module Advent.Y2022.Day1.Part2 where

import Advent.Input
import Data.Text hiding (take, reverse, map, maximum)
import Data.List.Split
import Data.List (sort)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2022/Day1/input"
  let ans = solve lines
  print ans

groupSum :: [Text] -> Int
groupSum lines = sum $ map (read . unpack) lines

solve :: [Text] -> Int
solve lines = sum sums
  where
    groups = splitWhen (== "") lines
    sums = take 3 $ reverse $ sort $ map groupSum groups
