{-# LANGUAGE OverloadedStrings #-}
module Advent.Y2021.Day2.Part2 where
import Advent.Input
import Relude.Unsafe (read)
import System.Directory.Internal.Prelude (foldl)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day2/input"
  print $ solve $ map toString lines


resolveCmd :: String -> (String, Integer)
resolveCmd cmd = let
    [dir, amountStr] = words $ toText cmd
    amount = read $ toString amountStr
    in
       (toString dir, amount)

accumulator :: (String, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
accumulator (cmd, x) (pa, px, py)
    | cmd == "down" = (pa + x, px, py)
    | cmd == "up" = (pa - x, px, py)
    | otherwise = (pa, px + x, py + (pa * x))

solve :: [String] -> Integer
solve = prod . foldl (flip (accumulator . resolveCmd)) (0, 0, 0)
    where
        prod (_, x, y) = x * y
