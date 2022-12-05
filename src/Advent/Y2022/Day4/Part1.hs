module Advent.Y2022.Day4.Part1 where
import Advent.Input (readInput)
import Data.Text (unpack)
import Data.List.Split (splitOn)
import Numeric.Interval (Interval, interval, contains)
import Data.Maybe (fromJust)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2022/Day4/input"
  print $ solve $ map unpack lines

parseLine :: String -> (Interval Int, Interval Int)
parseLine ln = (fromJust ai, fromJust bi)
    where
        [pa, pb] = splitOn "," ln
        [ax, ay] = splitOn "-" pa
        [bx, by] = splitOn "-" pb
        ai = interval (read ax) (read ay)
        bi = interval (read bx) (read by)

pairContains :: (Interval Int, Interval Int) -> Bool
pairContains (a, b) = (a `contains` b) || (b `contains` a)

solve :: [String] -> Int
solve = length . filter pairContains . map parseLine

