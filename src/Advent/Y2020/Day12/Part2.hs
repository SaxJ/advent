module Advent.Y2020.Day12.Part2 where

import Advent.Input
import Data.Text (unpack)
import Relude.Unsafe (read)

data Direction = E | S | W | N deriving (Enum, Show)

type Instruction = (Char, Integer)

type Position = (Integer, Integer)

type ShipState = (Integer, Position, Position)

parseInstruction :: String -> (Char, Integer)
parseInstruction (c : cs) = (c, read cs)

parseInput :: [Text] -> [(Char, Integer)]
parseInput = map (parseInstruction . unpack)

makeMove :: ShipState -> Instruction -> ShipState
makeMove (a, pos, (x, y)) ('N', n) = (a, pos, (x, y + n))
makeMove (a, pos, (x, y)) ('S', n) = (a, pos, (x, y - n))
makeMove (a, pos, (x, y)) ('E', n) = (a, pos, (x + n, y))
makeMove (a, pos, (x, y)) ('W', n) = (a, pos, (x - n, y))
makeMove (a, pos, wp) mv@('L', n) = (a - n, pos, rotation mv wp)
makeMove (a, pos, wp) mv@('R', n) = (a + n, pos, rotation mv wp)
makeMove (a, pos, wp) ('F', n) = (a, add pos $ mult n wp, wp)

add :: Position -> Position -> Position
add (a, b) (x, y) = (a + x, b + y)

angleToDirection :: Integer -> Direction
angleToDirection a = toEnum $ (a' `div` 90) `mod` 4
  where
    a' = fromInteger a

mult :: Integer -> Position -> Position
mult n (x, y) = (n * x, n * y)

execute :: [Instruction] -> ShipState
execute = foldl' makeMove initialShip
  where
    initialShip = (0, (0, 0), (10, 1))

manhatten :: ShipState -> Integer
manhatten (_, (x, y), _) = abs x + abs y

rotation :: (Char, Integer) -> (Integer, Integer) -> (Integer, Integer)
rotation (c, rot) (wx, wy)
  | r == 90 || r == 270 = (wy * (if r == 90 then 1 else -1), wx * (if r == 90 then -1 else 1))
  | r == 180 = (- wx, - wy)
  where
    r = if c == 'R' then rot else 360 - rot

solution :: IO ()
solution = do
  input <- readInput "src/Advent/Y2020/Day12/input"
  print $ manhatten $ execute $ parseInput input
