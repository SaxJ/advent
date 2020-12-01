module Advent.Y2020.Day1.Part1 where

import Advent.Input
import Data.List
import Data.Maybe
import Prelude hiding (lines)

solution :: IO ()
solution = do
  nums <- readIntegers "src/Advent/Y2020/Day1/input"
  print $ solve nums

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | (x, ix) <- zip xs [0 ..], (y, iy) <- zip ys [0 ..], ix /= iy]

solve :: [Integer] -> Integer
solve nums = x * y
  where
    cart = cartProd nums nums
    cond = \(a, b) -> a + b == 2020
    (x, y) = fromJust $ find cond cart
