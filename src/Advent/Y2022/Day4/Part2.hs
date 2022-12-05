module Advent.Y2022.Day4.Part2 where
import Advent.Input (readInput)
import Data.Text (unpack)
import Data.List.Split (splitOn)
import Numeric.Interval (Interval, interval, contains, intersection, null)
import Data.Maybe (fromJust)
import Prelude hiding (null)

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

overlap :: (Interval Int, Interval Int) -> Bool
overlap (a, b) = not $ null $ intersection a b

solve :: [String] -> Int
solve = length . filter overlap . map parseLine

