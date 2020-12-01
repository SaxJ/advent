module Advent.Y2020.Day1.Part2 where

import Advent.Input
import Data.List
import Data.Maybe
import Prelude hiding (lines)

solution :: IO ()
solution = do
  nums <- readIntegers "src/Advent/Y2020/Day1/input"
  print $ solve nums

triples :: [a] -> [b] -> [c] -> [(a, b, c)]
triples xs ys zs = [(x, y, z) | (x, ix) <- zip xs [0 ..], (y, iy) <- zip ys [0 ..], (z, iz) <- zip zs [0 ..], ix /= iy && ix /= iz]

solve :: [Integer] -> Integer
solve nums = x * y * z
  where
    trips = triples nums nums nums
    cond = \(a, b, c) -> a + b + c == 2020
    (x, y, z) = fromJust $ find cond trips
