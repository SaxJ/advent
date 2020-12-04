module Advent.Y2020.Day4.Part2 where

import Advent.Input (readInput)
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (unpack)
import Relude.Unsafe
import Text.Read
import Text.Regex.TDFA
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

requiredFieldsCheck :: Set String -> Bool
requiredFieldsCheck st = Set.isSubsetOf diff passingSet
  where
    diff = Set.difference allFieldsSet st

checkField :: Field -> Bool
checkField ("byr", v) = case readMaybe v :: Maybe Integer of
  Just x -> x <= 2002 && x >= 1920
  Nothing -> False
checkField ("iyr", v) = case readMaybe v :: Maybe Integer of
  Just x -> x <= 2020 && x >= 2010
  Nothing -> False
checkField ("eyr", v) = case readMaybe v :: Maybe Integer of
  Just x -> x <= 2030 && x >= 2020
  Nothing -> False
checkField ("hgt", v) = case (v =~ ("([0-9]+)(cm|in)" :: String)) :: (String, String, String, [String]) of
  (_, _, _, []) -> False
  (_, _, _, [height, unit]) -> checkHeight height unit
    where
      checkHeight h "cm" = (read h :: Int) <= 193 && (read h :: Int) >= 150
      checkHeight h "in" = (read h :: Int) <= 76 && (read h :: Int) >= 59
checkField ("hcl", v) = case (v =~ ("#([a-fA-F0-9]+)" :: String)) :: (String, String, String, [String]) of
  (_, _, _, []) -> False
  (_, _, _, [col]) -> length col == 6
checkField ("ecl", v) = v `Set.member` Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
checkField ("pid", v) = case (v =~ ("([0-9]+)" :: String)) :: (String, String, String, [String]) of
  (_, _, _, []) -> False
  (_, _, _, [id]) -> length id == 9
checkField ("cid", _) = True
checkField (_, _) = False

checkFields :: [Field] -> Bool
checkFields = all checkField

solve :: [Text] -> Int
solve = length . filter checkFields . filter (requiredFieldsCheck . fieldSet) . map fields . paragraphs

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day4/input"
  print $ solve lines
