module Advent.Y2020.Day9.Part1 where

import Advent.Input
import Data.Maybe
import Data.List (tails)

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

summingCheck :: [Integer] -> Bool
summingCheck window = any (\(x, y) -> x + y == target) couples
  where
    target = last window
    couples = [(x, y) | x <- window, y <- window, x /= y]

checkSeq :: [Integer] -> Bool
checkSeq = summingCheck

solve :: [Integer] -> Integer
solve input = last $ fst winningSeq
  where
    wins = windows 26 input
    couples = zip wins (map summingCheck wins)
    winningSeq = last $ filter (\(_, b) -> not b) couples

solution :: IO ()
solution = do
  input <- readIntegers "src/Advent/Y2020/Day9/input"
  print $ solve input
