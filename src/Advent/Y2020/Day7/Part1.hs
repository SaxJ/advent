module Advent.Y2020.Day7.Part1 where

import Advent.Input (readInput)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Text (unpack)
import Relude.Unsafe (read)
import Text.Regex.TDFA

type Child = (String, Int)

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
    children = map parseBagRule $ splitRuleList childList

getChildMap :: [Text] -> Map String [Child]
getChildMap = Map.fromList . map (parseLine . unpack)

search :: String -> Map String [Child] -> String -> Bool
search _ _ "shiny gold" = False
search lookingFor graph start = (length $ filter (== lookingFor) $ path graph [start] (map fst $ (Map.!) graph start)) > 0
  where
    path _ visited [] = reverse visited
    path graph visited (curr : next)
      | curr `elem` visited = path graph visited next
      | otherwise = path graph (curr : visited) (addToList graph curr next)

    addToList graph curr next = case Map.lookup curr graph of
      Just ls -> map fst ls ++ next
      Nothing -> next

solution :: IO ()
solution = do
  inputLines <- readInput "src/Advent/Y2020/Day7/input"
  let graph = getChildMap inputLines
  print $ length $ filter (== True) $ map (search "shiny gold" graph) $ Map.keys graph
