module Advent.Y2020.Day14.Part1 where

import Advent.Input
import Data.Char (intToDigit)
import qualified Data.Map as Map
import Data.Text (pack, unpack)
import Relude.Unsafe (read)
import Text.Regex.TDFA

type Rule = (Integer, Text)

type Program = (Text, [Text])

data Cmd = Mask Text | Mem Rule deriving (Show)

padL :: a -> Int -> [a] -> [a]
padL p s l
  | length l >= s = l
  | otherwise = replicate (s - length l) p ++ l

ruleRegex = "mem\\[([0-9]+)\\] = ([0-9]+)" :: String

maskRegex = "mask = ([01X]+)" :: String

stringSize = 36

parseRule :: Text -> Cmd
parseRule txt =
  if null mem
    then Mask (pack mask)
    else case mem of
      [address, n] -> Mem (read address, pack $ padL '0' stringSize $ unpack $ encode $ read n)
      _ -> error "It's fucked"
  where
    (_, _, _, mem) = unpack txt =~ ruleRegex :: (String, String, String, [String])
    (_, _, _, [mask]) = unpack txt =~ maskRegex :: (String, String, String, [String])

parseRules :: [Text] -> [Cmd]
parseRules = map parseRule

decode :: Text -> Integer
decode = foldr f 0 . reverse . zip [0 ..] . reverse . unpack
  where
    f (i, c) acc = if c == '0' then acc else acc + 2 ^ i

encode :: Integer -> Text
encode = pack . map intToDigit . bin
  where
    bin 0 = [0]
    bin n
      | odd n = bin (n `div` 2) ++ [1]
      | even n = bin (n `div` 2) ++ [0]

applyMask :: Text -> Text -> Text
applyMask mask num = pack $ zipWith result (unpack mask) (unpack num)
  where
    result m x = case m of
      'X' -> x
      _ -> m

runProgram :: [Cmd] -> (Text, Map Integer Integer)
runProgram rules = foldl' fun ("", Map.empty) rules
  where
    fun (mask, accMap) cmd = case cmd of
      Mask m -> (m, accMap)
      Mem (a, n) -> (mask, Map.insert a (decode $ applyMask mask n) accMap)

solve :: [Text] -> Integer
solve = sum . Map.elems . snd . runProgram . parseRules

solution :: IO ()
solution = do
  rules <- readInput "src/Advent/Y2020/Day14/input"
  print $ solve rules
