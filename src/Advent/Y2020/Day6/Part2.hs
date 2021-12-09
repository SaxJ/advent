module Advent.Y2020.Day6.Part2 where

import Advent.Input (readInput)
import Data.List.Split
import qualified Data.Set as Set
import Data.Text (unpack, Text)
import Data.Set (Set)

type Paragraph = [Text]

paragraphs :: [Text] -> [Paragraph]
paragraphs = splitWhen (== "")

intersections :: Ord a => [Set a] -> Set a
intersections sets = foldr Set.intersection (Set.unions sets) sets

questionsAnswered :: Paragraph -> Int
questionsAnswered = Set.size . intersections . map (Set.fromList . unpack)

solve :: [Text] -> Int
solve = sum . map questionsAnswered . paragraphs

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day6/input"
  print $ solve lines
