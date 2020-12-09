module Advent.Y2020.Day6.Part1 where

import Advent.Input (readInput)
import Data.List.Split
import qualified Data.Set as Set
import Data.Text (unpack)

type Paragraph = [Text]

paragraphs :: [Text] -> [Paragraph]
paragraphs = splitWhen (== "")

questionsAnswered :: Paragraph -> Int
questionsAnswered = Set.size . Set.unions . map (Set.fromList . unpack)

solve :: [Text] -> Int
solve = sum . map questionsAnswered . paragraphs

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day6/input"
  print $ solve lines
