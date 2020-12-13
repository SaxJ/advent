module Advent.Y2020.Day10.Part2 where

import Advent.Input
import Data.List (maximum)
import qualified Data.Set as Set

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

optionalAdapters :: [Integer] -> Set Integer
optionalAdapters = Set.fromList . map (\[_, x, _] -> x) . filter (\[a, _, c] -> a - c <= 3) . windows 3 . reverse . sort

solve :: Set Integer -> Integer
solve options = foldr accumulator 1 options
  where
    accumulator curr acc
      | (curr + 1) `Set.member` options && (curr + 2) `Set.member` options = acc + (3 * acc `div` 4)
      | otherwise = acc + acc

solution :: IO ()
solution = do
  input <- readIntegers "src/Advent/Y2020/Day10/input"
  print $ solve $ optionalAdapters (0 : maximum input + 3 : input)
