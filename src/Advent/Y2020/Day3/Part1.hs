module Advent.Y2020.Day3.Part1 where

import Advent.Input (readInput)
import Data.Text (unpack)
import Relude.Unsafe

solve :: [String] -> Int
solve = length . hits . positions

positions :: [String] -> [(Int, String)]
positions strs = map (\(p, s) -> (p `mod` length s, s)) pairs
  where
    pairs = zip [0, 3 ..] strs

hits :: [(Int, String)] -> [(Int, String)]
hits = filter check
  where
    check (p, s) = s !! p == '#'

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day3/input"
  print $ solve $ map unpack lines
