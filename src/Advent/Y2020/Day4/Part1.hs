module Advent.Y2020.Day4.Part1 where

import Advent.Input (readInput)
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (unpack)
import Relude.Unsafe
import Text.Scanf

type Paragraph = [Text]

type Field = (String, String)

allFieldsSet = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

passingSet = Set.fromList ["cid"]

fieldFormat = fmt_ (string . ":" % string)

parseField :: String -> Field
parseField field = (key, val)
  where
    [key, val] = splitOn ":" field

parseLine :: Text -> [Field]
parseLine = map (parseField . unpack) . words

fields :: Paragraph -> [Field]
fields = concatMap parseLine

paragraphs :: [Text] -> [Paragraph]
paragraphs = splitWhen (== "")

fieldSet :: [Field] -> Set String
fieldSet = Set.fromList . map fst

validityCheck :: Set String -> Bool
validityCheck st = Set.isSubsetOf diff passingSet
  where
    diff = Set.difference allFieldsSet st

solve :: [Text] -> Int
solve = length . filter validityCheck . map (fieldSet . fields) . paragraphs

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day4/input"
  print $ solve lines
