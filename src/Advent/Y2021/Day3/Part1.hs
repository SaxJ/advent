module Advent.Y2021.Day3.Part1 where
import Advent.Input (readInput )
import System.Directory.Internal.Prelude ((!!), Foldable (maximum, minimum))
import qualified Data.Map as Map
import Relude.Unsafe (fromJust)
import Data.Char (digitToInt)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day3/input.test"
  print $ solve $ map toString lines

atIndex :: Int -> [String] -> [Char]
atIndex x = map (!! x)

countOccurances :: [Char] -> Map Char Integer
countOccurances = foldr accumulator Map.empty
  where
    accumulator x acc = Map.insertWith (+) x 1 acc

maxValueKey mp = fromJust $ viaNonEmpty head $ Map.keys $ Map.filter (== m) mp
        where
          m = maximum $ Map.elems mp

minValueKey mp = fromJust $ viaNonEmpty head $ Map.keys $ Map.filter (== m) mp
        where
          m = minimum $ Map.elems mp


gamma :: [String] -> String
gamma inputs = [mostCommonAtIndex x inputs | x <- [0..11]]
  where
    mostCommonAtIndex x lst = maxValueKey $ countOccurances $ atIndex x lst

epsilon :: [String] -> String
epsilon inputs = [leastCommonAtIndex x inputs | x <- [0..11]]
  where
    leastCommonAtIndex x lst = minValueKey $ countOccurances $ atIndex x lst

solve :: [String] -> Integer
solve inputs =  x * y
  where
    x = bintodec $ gamma inputs
    y = bintodec $ epsilon inputs


pwr :: (Integer, Char) -> Integer
pwr (i, c)
        | c == '0' = 0
        | otherwise = 2 ^ i

bintodec :: String -> Integer
bintodec str = sum powers
  where
    powers = zipWith (curry pwr) [0..] (reverse str)
