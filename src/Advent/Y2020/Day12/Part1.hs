{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Advent.Y2020.Day12.Part1 where

import Advent.Input
import Data.Text (unpack, Text)
import GHC.List (foldl')

data Direction = E | S | W | N deriving (Enum)

type Instruction = (Char, Int)

type Position = (Int, Int)

type ShipState = (Int, Position)

angleToDirection :: Int -> Direction
angleToDirection a = toEnum $ (a `div` 90) `mod` 4

parseInstruction :: String -> (Char, Int)
parseInstruction (c : cs) = (c, read cs)

parseInput :: [Text] -> [(Char, Int)]
parseInput = map (parseInstruction . unpack)

makeMove :: ShipState -> Instruction -> ShipState
makeMove (a, (x, y)) ('N', n) = (a, (x, y - n))
makeMove (a, (x, y)) ('S', n) = (a, (x, y + n))
makeMove (a, (x, y)) ('E', n) = (a, (x + n, y))
makeMove (a, (x, y)) ('W', n) = (a, (x - n, y))
makeMove (a, pos) ('L', n) = (a - n, pos)
makeMove (a, pos) ('R', n) = (a + n, pos)
makeMove ship@(a, _) ('F', n) = makeMove ship (dir, n)
  where
    dir = case angleToDirection a of
      E -> 'E'
      S -> 'S'
      W -> 'W'
      N -> 'N'

execute :: [Instruction] -> ShipState
execute = foldl' makeMove initialShip
  where
    initialShip = (0, (0, 0))

manhatten :: ShipState -> Int
manhatten (_, (x, y)) = abs x + abs y

solution :: IO ()
solution = do
  input <- readInput "src/Advent/Y2020/Day12/input"
  print $ manhatten $ execute $ parseInput input
