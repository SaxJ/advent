module Advent.Y2020.Day1.Part1 where

import Data.List
import Data.Maybe
import Relude.Unsafe (read)
import Prelude hiding (lines)

solution :: IO ()
solution = do
  input <- readFile "src/Advent/Y2020/Day1/input"
  print $ solve $ lines input

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | (x, ix) <- zip xs [0 ..], (y, iy) <- zip ys [0 ..], ix /= iy]

solve :: [String] -> Int
solve elements = x * y
  where
    nums = map read elements :: [Int]
    cart = cartProd nums nums
    cond = \(a, b) -> a + b == 2020
    (x, y) = fromJust $ find cond cart
