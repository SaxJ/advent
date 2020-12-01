module Advent.Y2020.Day1.Part2 where

import Data.List
import Data.Maybe
import Relude.Unsafe (read)
import Prelude hiding (lines)

solution :: IO ()
solution = do
  input <- readFile "src/Advent/Y2020/Day1/input"
  print $ solve $ lines input

triples :: [a] -> [b] -> [c] -> [(a, b, c)]
triples xs ys zs = [(x, y, z) | (x, ix) <- zip xs [0 ..], (y, iy) <- zip ys [0 ..], (z, iz) <- zip zs [0 ..], ix /= iy && ix /= iz]

solve :: [String] -> Int
solve elements = x * y * z
  where
    nums = map read elements :: [Int]
    trips = triples nums nums nums
    cond = \(a, b, c) -> a + b + c == 2020
    (x, y, z) = fromJust $ find cond trips
