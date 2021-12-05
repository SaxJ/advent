{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Advent.Y2021.Day3.Part2 where
import Advent.Input (readInput, countOccurances, bintodec)
import Advent.Y2021.Day3.Part1 (atIndex)
import Data.List ( foldl, (!!), head, maximum, minimum )
import Relude hiding (head)
import Prelude hiding (head)
import qualified Data.Map as Map

indexes = [0..11]

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day3/input"
  print $ solve $ map toString lines

maxValueKey mp
  | length ks > 1 = '1'
  | otherwise = head ks
        where
          m = maximum $ Map.elems mp
          ks = Map.keys $ Map.filter (== m) mp

minValueKey mp
  | length ks > 1 = '0'
  | otherwise = head ks
        where
          m = minimum $ Map.elems mp
          ks = Map.keys $ Map.filter (== m) mp

mostCommonAtIndex :: Int -> [String] -> Char
mostCommonAtIndex idx = maxValueKey . countOccurances . atIndex idx

leastCommonAtIndex :: Int -> [String] -> Char
leastCommonAtIndex idx = minValueKey . countOccurances . atIndex idx

gamma :: [String] -> String
gamma inputs = [mostCommonAtIndex x inputs | x <- indexes]
  where
    mostCommonAtIndex x lst = maxValueKey $ countOccurances $ atIndex x lst

epsilon :: [String] -> String
epsilon inputs = [leastCommonAtIndex x inputs | x <- indexes]
  where
    leastCommonAtIndex x lst = minValueKey $ countOccurances $ atIndex x lst

oxygenFilter = gamma
co2Filter = epsilon

oxygen :: [[Char]] -> [[Char]]
oxygen lines = foldl filterChain lines indexes
  where
    filterChain acc idx = case acc of
      [] -> []
      [a] -> [a]
      _ -> filter (\x -> x !! idx == bitFilter !! idx) acc
      where
        bitFilter = oxygenFilter acc

co2 :: [[Char]] -> [[Char]]
co2 lines = foldl filterChain lines indexes
  where
    filterChain acc idx = case acc of
      [] -> []
      [a] -> [a]
      _ -> filter (\x -> x !! idx == bitFilter !! idx) acc
      where
        bitFilter = co2Filter acc

solve lines = o * c
  where
    o = bintodec $ head $ oxygen lines
    c = bintodec $ head $ co2 lines
