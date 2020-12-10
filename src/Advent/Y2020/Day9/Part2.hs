module Advent.Y2020.Day9.Part2 where

import Advent.Input
import Data.List (maximum, minimum)
import Data.Maybe

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

summingCheck :: [Integer] -> Bool
summingCheck window = any (\(x, y) -> x + y == target) couples
  where
    Just target = viaNonEmpty last window
    couples = [(x, y) | x <- window, y <- window, x /= y]

checkSeq :: [Integer] -> Bool
checkSeq xs = case nonEmpty xs of
  Just window -> summingCheck $ toList window
  Nothing -> False

getTarget :: [Integer] -> Maybe Integer
getTarget input = viaNonEmpty last $ fst $ fromJust winningSeq
  where
    wins = windows 26 input
    couples = zip wins (map summingCheck wins)
    winningSeq = viaNonEmpty last $ filter (\(_, b) -> not b) couples

subseqs :: [a] -> [[a]]
subseqs = filter (not . null) . concatMap inits . tails

solve :: Integer -> [Integer] -> Integer
solve target input = minimum win + maximum win
  where
    win = fromJust $ find (\l -> sum l == target) $ subseqs input

solution :: IO ()
solution = do
  input <- readIntegers "src/Advent/Y2020/Day9/input"
  let target = getTarget input
  print $ solve (fromJust target) input
