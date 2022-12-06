module Advent.Y2022.Day6.Part2 where
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
            Just i -> 14 + i
    where
        mi = findIndex uniqueWindow $ windows 14 s

uniqueWindow :: String -> Bool
uniqueWindow s = (length $ nub s) == 14

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails
