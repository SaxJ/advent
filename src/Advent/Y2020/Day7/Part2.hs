module Advent.Y2020.Day7.Part2 where

import Advent.Input (readInput)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (unpack)
import Relude.Unsafe (read)
import Text.Regex.TDFA

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
    children = map parseBagRule $ splitRuleList childList

getChildMap :: [Text] -> Map String [Child]
getChildMap = Map.fromList . map (parseLine . unpack)

path _ visited [] total = total
path graph visited (currEl@(curr, cnt) : next) total
  | curr `elem` visited = total + path graph visited next
  | otherwise = path graph (curr : visited) (addToList graph currEl next)
  where
    addToList graph (curr, _) next = case Map.lookup curr graph of
      Just ls -> ls ++ next
      Nothing -> next

bagCount :: Graph -> String -> Int
bagCount graph bag = sum $ map (\(b, c) -> c * (bagCount graph b)) $ (Map.!) graph bag

solution :: IO ()
solution = do
  inputLines <- readInput "src/Advent/Y2020/Day7/input"
  let graph = getChildMap inputLines
  let shinyPath = path graph ["shiny gold"] ((Map.!) graph "shiny gold")
  print "hello"
