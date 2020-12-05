module Advent.Y2020.Day5.Part1 where

import Advent.Input (readInput)
import Data.Text (unpack)
import GHC.List (maximum)

binaryPartition :: (Int, Int, Int, Int) -> Char -> (Int, Int, Int, Int)
binaryPartition (ra, rb, ca, cb) char
  | char == 'F' = (ra, (ra + rb) `div` 2, ca, cb)
  | char == 'B' = ((ra + rb) `div` 2 + 1, rb, ca, cb)
  | char == 'L' = (ra, rb, ca, (ca + cb) `div` 2)
  | otherwise = (ra, rb, (ca + cb) `div` 2 + 1, cb)

getPosition :: String -> (Int, Int)
getPosition line = (rowMin, colMin)
  where
    (rowMin, _, colMin, _) = foldl' binaryPartition (0, 127, 0, 7) line

seatId :: (Int, Int) -> Int
seatId (r, c) = r * 8 + c

solve :: [Text] -> Int
solve = maximum . map (seatId . getPosition . unpack)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day5/input"
  print $ solve lines
