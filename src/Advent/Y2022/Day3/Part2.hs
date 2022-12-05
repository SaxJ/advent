module Advent.Y2022.Day3.Part2 where

import Data.Text (length, Text, unpack)
import Advent.Input (readInput)
import Prelude hiding (length)
import Data.Char (ord, isLower)
import Data.List (intersect)
import Data.List.Split (chunksOf)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2022/Day3/input"
  print $ solve $ map unpack lines

priority :: Char -> Int
priority c
    | isLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

common :: Eq a => [[a]] -> a
common [a, b, c] = head $ intersect a $ intersect b c

solve :: [String] -> Int
solve = sum . (map (priority. common)) . chunksOf 3
