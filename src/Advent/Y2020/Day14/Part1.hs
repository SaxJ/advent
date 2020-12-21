module Advent.Y2020.Day14.Part1 where

import Advent.Input
import Data.Char (intToDigit)
import qualified Data.Map as Map
import Data.Text (pack, unpack)
import Relude.Unsafe (read)
import Text.Regex.TDFA

type Rule = (Integer, Text)

type Program = (Text, [Text])

padL :: a -> Int -> [a] -> [a]
padL p s l
  | length l >= s = l
  | otherwise = replicate (s - length l) p ++ l

ruleRegex = "mem\\[([0-9]+)\\] = ([01]+)" :: String

maskRegex = "mask = ([01X]+)" :: String

parseRules :: [Text] -> [Rule]
parseRules = map parseRule
  where
    parseRule rule = (read address, pack $ padL '0' 32 $ unpack $ encode $ read num)
      where
        (_, _, _, [address, num]) = unpack rule =~ ruleRegex :: (String, String, String, [String])

decode :: Text -> Integer
decode = foldr f 0 . reverse . zip [0 ..] . reverse . unpack
  where
    f (i, c) acc = if c == '0' then 0 else acc + 2 ^ i

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

runProgram :: Text -> [Rule] -> Map Integer Integer
runProgram mask rules = foldl' fun Map.empty rules
  where
    fun accMap (mem, num) = Map.insert mem newNum accMap
      where
        newNum = decode $ applyMask mask num

solve :: Text -> [Text] -> Integer
solve mask = sum . Map.elems . runProgram mask . parseRules

solution :: IO ()
solution = do
  (maskLine : rules) <- readInput "src/Advent/Y2020/Day14/test"
  let (_, _, _, [mask]) = unpack maskLine =~ maskRegex :: (String, String, String, [String])
  print $ solve (pack mask) rules
