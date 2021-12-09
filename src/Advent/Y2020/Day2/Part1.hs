module Advent.Y2020.Day2.Part1 where

import Advent.Input
import Data.Text (unpack, Text)
import Text.Scanf

ruleFormat = fmt_ (int . "-" % int . " " % char . ": " % string)

parseRule :: String -> (Int, Int, Char, String)
parseRule rule = (min, max, ch, str)
  where
    Just (min :+ max :+ ch :+ str :+ ()) = scanf ruleFormat rule

count :: Char -> String -> Int
count c = foldr (\x acc -> if x == c then acc + 1 else acc) 0

checkRule :: (Int, Int, Char, String) -> Bool
checkRule (min, max, char, str) = cnt <= max && cnt >= min
  where
    cnt = count char str

solve :: [Text] -> Int
solve = length . filter checkRule . map (parseRule . unpack)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day2/input"
  print $ solve lines
