module Advent.Y2022.Day6.Part1 where
import Data.List (tails, nub)
import Data.Text (unpack)
import Advent.Input (readInput)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

solution :: IO ()
solution = do
    lines <- readInput "src/Advent/Y2022/Day6/input"
    print $ solve $ head $ map unpack lines

solve :: String -> Int
solve s = case mi of
            Nothing -> (-1)
            Just i -> 4 + i
    where
        mi = findIndex uniqueWindow $ windows 4 s

uniqueWindow :: String -> Bool
uniqueWindow s = (length $ nub s) == 4

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails
