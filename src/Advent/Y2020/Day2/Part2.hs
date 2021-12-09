module Advent.Y2020.Day2.Part2 where

import Advent.Input
import Advent.Y2020.Day2.Part1 (parseRule)
import Data.Text (unpack, Text)

checkRule :: (Int, Int, Char, String) -> Bool
checkRule (p1, p2, char, str) = con1 || con2
  where
    x = str !! (p1 - 1)
    y = str !! (p2 - 1)
    con1 = x == char && y /= char
    con2 = x /= char && y == char

solve :: [Text] -> Int
solve = length . filter checkRule . map (parseRule . unpack)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day2/input"
  print $ solve lines
