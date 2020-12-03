module Advent.Y2020.Day3.Part2 where

import Advent.Input (readInput)
import Data.Text (unpack)
import Relude.Unsafe

solve :: [String] -> Int
solve = product . map (length . hits . normalise) . pairsLists

normalise :: [(Int, String)] -> [(Int, String)]
normalise = map (\(p, s) -> (p `mod` length s, s))

pairsLists :: [String] -> [[(Int, String)]]
pairsLists strs = [zip [0, 1 ..] strs, zip [0, 3 ..] strs, zip [0, 5 ..] strs, zip [0, 7 ..] strs, zip [0, 1 ..] evens]
  where
    indexes = zip strs ([0 ..] :: [Int])
    evens = map fst $ filter (\(_, i) -> even i) indexes

hits :: [(Int, String)] -> [(Int, String)]
hits = filter check
  where
    check (p, s) = s !! p == '#'

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day3/input"
  print $ solve $ map unpack lines
