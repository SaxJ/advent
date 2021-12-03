module Advent.Y2021.Day1.Part1 where

import Advent.Input
import Data.List
import Data.Maybe
import Prelude hiding (lines)

solution :: IO ()
solution = do
  nums <- readIntegers "src/Advent/Y2021/Day1/input.txt"
  print $ solve nums

solve :: [Integer] -> Integer
solve (x:y:xs)
    | x < y = 1 + solve (y:xs)
    | otherwise = solve (y:xs)
solve (x:xs) = solve xs
solve [] = 0
