module Advent.Y2020.Day7.Part2 where

import Advent.Input (readInput)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Text (unpack, Text)
import Text.Regex.TDFA
import Data.Map (Map)

type Child = (String, Int)

type Graph = Map String [Child]

ruleLineFormat = "([0-9a-z ,]+) bags contain ([0-9a-z ,]+)." :: String

ruleRegexFormat = "([0-9]+) ([a-z ]+) bag([s]*)" :: String

parseBagRule :: String -> Child
parseBagRule "no other bags" = ("", 0)
parseBagRule rule = (colour, read countStr)
  where
    (_, _, _, [countStr, colour, _]) = rule =~ ruleRegexFormat :: (String, String, String, [String])

splitRuleList :: String -> [String]
splitRuleList = splitOn ", "

parseLine :: String -> (String, [Child])
parseLine line = (parent, children)
  where
    (_, _, _, [parent, childList]) = line =~ ruleLineFormat :: (String, String, String, [String])
    children = filter ((/= "") . fst) $ map parseBagRule $ splitRuleList childList

getChildMap :: [Text] -> Map String [Child]
getChildMap = Map.fromList . map (parseLine . unpack)

bagCount :: Graph -> String -> Int
bagCount graph bag = 1 + sum (map (\(b, c) -> c * bagCount graph b) childs)
  where
    childs = (Map.!) graph bag

solution :: IO ()
solution = do
  inputLines <- readInput "src/Advent/Y2020/Day7/input"
  let graph = getChildMap inputLines
  print $ bagCount graph "shiny gold" - 1
