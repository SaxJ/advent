module Advent.Y2020.Day13.Part1 where

import Advent.Input
import Data.List (minimumBy, find)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (unpack, Text)
import Data.Map (Map)

busTimes :: Integer -> [Integer]
busTimes x = [0, x ..]

parseBusId :: String -> Maybe Integer
parseBusId x
  | x == "x" = Nothing
  | otherwise = Just $ read x

parseIdList :: Text -> [Maybe Integer]
parseIdList = map parseBusId . split'

split' :: Text -> [String]
split' = splitOn "," . unpack

sequenceMap :: [Maybe Integer] -> Map Integer [Integer]
sequenceMap maybes = Map.fromList $ zip ls $ map busTimes ls
  where
    ls = catMaybes maybes

firstOvers :: Integer -> Map Integer [Integer] -> Map Integer Integer
firstOvers goal = Map.map mp
  where
    mp = fromJust . find (>= goal)

solve :: Text -> Text -> (Integer, Integer)
solve goal = minimum' . firstOvers (read $ unpack goal) . sequenceMap . parseIdList

answer :: Text -> Text -> Integer
answer goal line = id * (time - g)
  where
    g = read $ unpack goal
    (id, time) = solve goal line

minimum' :: Map Integer Integer -> (Integer, Integer)
minimum' = minimumBy comp . Map.toList
  where
    comp (_, a) (_, b) = compare a b

solution :: IO ()
solution = do
  [goal, line] <- readInput "src/Advent/Y2020/Day13/input"
  print $ answer goal line
