module Advent.Y2021.Day6.Part1 where
import Advent.Input (readInput)
import Data.Text (unpack, pack, splitOn)
import Debug.Trace (trace)

step :: [Integer] -> [Integer]
step input = map dec input ++ replicate zs 8
  where
    zs = length $ filter (== 0) input
    dec x
      | x == 0 = 6
      | otherwise = x - 1

solve input = length $ foldr (\_ acc -> step acc) start [1..256]
  where
    start = [read x | x <- map unpack $ splitOn "," $ pack input]

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day6/input"
  print $ solve $ head $ map unpack lines
