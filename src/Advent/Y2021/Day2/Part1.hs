{-# LANGUAGE OverloadedStrings #-}
module Advent.Y2021.Day2.Part1 where
import Advent.Input ( readInput )
import Data.Text (unpack)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day2/input"
  print $ solve $ map unpack lines

resolveChange :: String -> (Integer, Integer)
resolveChange cmd = let
    [dir, amountStr] = words  cmd
    amount = read amountStr
    in
        case (dir, amount) of
            ("forward", x) -> (x, 0)
            ("down", x) -> (0, x)
            (_, x) -> (0, negate x)

solve :: [String] -> Integer
solve = prod . foldr (tupleSum . resolveChange) (0, 0)
    where
        tupleSum (a,b) (x, y) = (a + x, b + y)
        prod (x, y) = x * y
