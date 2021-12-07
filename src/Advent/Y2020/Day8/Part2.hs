{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Advent.Y2020.Day8.Part2 where

import Advent.Input
import qualified Data.Sequence as Seq
import Data.Text (unpack, Text)
import System.Directory.Internal.Prelude (Foldable (maximum))
import Text.Regex.TDFA
import Data.Foldable (toList)

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
      | curr `elem` visited = -1
      | curr >= length program = acc
      | otherwise = case program !! curr of
        ("nop", _) -> programStepper acc (curr : visited) (curr + 1)
        ("acc", n) -> programStepper (acc + n) (curr : visited) (curr + 1)
        ("jmp", n) -> programStepper acc (curr : visited) (curr + n)

permutations' :: [Command] -> [[Command]]
permutations' cmds = map (toList . makeChange) [0 .. length cmds]
  where
    permute (cmd, cnt)
      | cmd == "nop" = ("jmp", cnt)
      | cmd == "jmp" = ("nop", cnt)
      | otherwise = (cmd, cnt)
    makeChange idx = Seq.adjust' permute idx $ Seq.fromList cmds

solve :: [Text] -> Int
solve = maximum . map execute . permutations' . parseInput

solution :: IO ()
solution = do
  inputLines <- readInput "src/Advent/Y2020/Day8/input"
  print $ solve inputLines
