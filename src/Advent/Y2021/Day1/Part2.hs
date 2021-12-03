module Advent.Y2021.Day1.Part2 where
import Advent.Input (readIntegers)
import Text.Regex.TDFA.CorePattern (P(Seq))

solution :: IO ()
solution = do
    nums <- readIntegers "src/Advent/Y2021/Day1/input.txt"
    print $ solve nums

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

increasing :: Ord b => (a -> b) -> [a] -> Integer
increasing valueFunc (x:y:xs)
    | valueFunc x < valueFunc y = 1 + increasing valueFunc (y:xs)
    | otherwise = increasing valueFunc (y:xs)
increasing f (x:xs) = increasing f xs
increasing _ [] = 0

solve :: [Integer] -> Integer 
solve nums = let
    wins = windows 3 nums
    in
        increasing sum wins
