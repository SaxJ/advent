module Advent.Y2020.Day5.Part2 where

import Advent.Input (readInput)
import qualified Data.Set as Set
import Data.Text (unpack, Text)
import GHC.List (maximum)
import Data.List (foldl', sort)

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

tickets :: [Text] -> [Int]
tickets = sort . map (seatId . getPosition . unpack)

solve :: [Text] -> [(Int, Int)]
solve lines = filter (\(_, x) -> x == 2) $ zip (tickets lines) (differences $ tickets lines)

differences :: [Int] -> [Int]
differences (a : b : xs) = abs (a - b) : differences (b : xs)
differences [a] = [a]
differences [] = []

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day5/input"
  case solve lines of
    [(x, _)] -> print (x + 1)
    _ -> print "Fail"
