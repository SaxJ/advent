module Advent.Y2022.Day5.Part1 where
import Advent.Input (readInput)
import Data.Text (unpack)
import Data.List.Split (splitWhen)
import Data.Char (isNumber, isAlpha)
import Safe (atMay)
import Data.Maybe (fromMaybe)

type Command = (Int, Int, Int)
type Stacks = [[Char]]

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2022/Day5/input"
  print $ solve $ map unpack lines

sections :: [String] -> ([String], [String])
sections lines = (a, b)
    where
        [a, b] = splitWhen (== "") lines

boxIndexes :: String -> [Int]
boxIndexes = map fst . filter (isNumber . snd) . zip [0..]

getColumn :: [String] -> Int -> [Char]
getColumn lines c = filter isAlpha col
    where
        mCol = map (`atMay` c) lines
        col = map (fromMaybe ' ') mCol

parseStacks :: [String] -> Stacks
parseStacks lines = map (getColumn lines) indexes
    where
        indexes = boxIndexes $ last lines

runCommand :: Stacks -> Command -> Stacks
runCommand stacks (num, from, to) = nextStack
    where
        mapper (idx, stack)
            | idx == from = drop num stack
            | idx == to = reverse (take num (stacks !! (from - 1))) ++ stack
            | otherwise = stack
        nextStack = zipWith (curry mapper) [1..] stacks

parseCommand :: String -> Command
parseCommand line = (read a, read b, read c)
    where
        parts = words line
        a = parts !! 1
        b = parts !! 3
        c = parts !! 5

runCommands :: [String] -> Stacks
runCommands lines = Prelude.foldl runCommand initialStack commands
    where
        (stackStrings, commandStrings) = sections lines
        initialStack = parseStacks stackStrings
        commands = map parseCommand commandStrings

solve :: [String] -> String
solve lines = map head stacks
    where
        stacks = runCommands lines
