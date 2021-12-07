module Advent.Y2021.Day3.Part1 where
import Advent.Input (readInput, bintodec, countOccurances )
import System.Directory.Internal.Prelude ((!!), Foldable (maximum, minimum))
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.Text (unpack)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day3/input"
  print $ solve $ map unpack lines

atIndex :: Int -> [String] -> [Char]
atIndex x = map (!! x)

maxValueKey mp = head $ Map.keys $ Map.filter (== m) mp
        where
          m = maximum $ Map.elems mp

minValueKey mp = head $ Map.keys $ Map.filter (== m) mp
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
