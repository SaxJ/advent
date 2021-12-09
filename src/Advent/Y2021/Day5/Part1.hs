{-# LANGUAGE OverloadedStrings #-}
module Advent.Y2021.Day5.Part1 where
import Data.Text (splitOn, Text, unpack)
import Advent.Input (readInput, countOccurances)
import Text.Regex.TDFA
import qualified Data.Map as M
import Debug.Trace

type Rule = ((Integer, Integer), (Integer, Integer))
type Coord = (Integer, Integer)

ruleFormat = "[0-9]+" :: String

ruleToCoords :: Rule -> [Coord]
ruleToCoords rule@((a, b), (c, d))
  | a == c || b == d = [(x, y) | x <- [min a c..max a c], y <- [min b d..max b d]]
  | otherwise = []

parseLine :: Text -> ((Integer, Integer), (Integer, Integer))
parseLine line = ((a, b), (c, d))
  where
    matches = getAllTextMatches (unpack line =~ ruleFormat) :: [String]
    (a, b, c, d) = case matches of
      [q, w, e, r] -> (read q, read w, read e, read r)
      _ -> (0, 0, 0, 0)

solve = length . filter (\(_, x) -> x > 1) . M.toList . countOccurances . concatMap (ruleToCoords . parseLine)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day5/input"
  print $ solve lines
