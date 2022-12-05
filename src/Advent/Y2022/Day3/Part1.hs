module Advent.Y2022.Day3.Part1 where

import Data.Text (length, Text, unpack)
import Advent.Input (readInput)
import Prelude hiding (length)
import Data.Char (ord, isLower)
import Data.List (intersect)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2022/Day3/input"
  print $ solve lines

compartments :: Text -> ([Char], [Char])
compartments line = splitAt x (unpack line)
  where
    x = length line `div` 2

commonItem :: ([Char], [Char]) -> Char
commonItem (as, bs) = head $ intersect as bs

priority :: Char -> Int
priority c
    | isLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

solve :: [Text] -> Int
solve = sum . map (priority . commonItem . compartments)
