{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Advent.Y2020.Day8.Part1 where

import Advent.Input
import Data.Bool (bool)
import Data.Text (unpack, Text)
import Text.Regex.TDFA

type Command = (String, Int)

commandRegex :: String
commandRegex = "([a-z]+) ([+\\-])([0-9]+)"

parseCommand :: String -> Command
parseCommand line = (action, count)
  where
    (_, _, _, [action, mult, cnt]) = line =~ commandRegex :: (String, String, String, [String])
    factor = if mult == "-" then -1 else 1
    count = factor * read cnt :: Int

parseInput :: [Text] -> [Command]
parseInput = map (parseCommand . unpack)

execute :: [Command] -> Int
execute [] = 0
execute program = programStepper 0 [] 0
  where
    programStepper acc visited curr
      | curr `elem` visited = acc
      | otherwise = case program !! curr of
        ("nop", _) -> programStepper acc (curr : visited) (curr + 1)
        ("acc", n) -> programStepper (acc + n) (curr : visited) (curr + 1)
        ("jmp", n) -> programStepper acc (curr : visited) (curr + n)

solve :: [Text] -> Int
solve = execute . parseInput

solution :: IO ()
solution = do
  inputLines <- readInput "src/Advent/Y2020/Day8/input"
  print $ solve inputLines
