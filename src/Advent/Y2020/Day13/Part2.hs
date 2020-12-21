{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2020.Day13.Part2 where

import Advent.Input
import Data.List (minimumBy)
import Data.List.Split
import Data.Text (unpack)
import Relude.Unsafe (fromJust, read)

busTimes :: Integer -> [Integer]
busTimes x = [0, x ..]

lcmBuses :: [Integer] -> Integer
lcmBuses = foldr lcm 1

parseBusId :: String -> Maybe Integer
parseBusId x
  | x == "x" = Nothing
  | otherwise = Just $ read x

parseIdList :: Text -> [(Integer, Integer)]
parseIdList txt = map (\(Just x, i) -> (x, i)) $ filter (isJust . fst) indexed
  where
    mIds = map parseBusId . split' $ txt
    indexed = zip mIds [0 ..]

split' :: Text -> [String]
split' = splitOn "," . unpack

crt :: [Integer] -> Integer -> [Integer] -> Integer
crt diffs mprod ids =
  let ins = zip diffs ids
   in foldr (\(x, y) r -> r + aux x y) 0 ins `mod` mprod
  where
    aux x y =
      let f = (mprod `div` y) `inv` y
       in ((x * mprod) `div` y) * f
    -- https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
    a `inv` m = let (_, i, _) = gcd' a m in i `mod` m
    -- https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell
    gcd' 0 b = (b, 0, 1)
    gcd' a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd' (b `mod` a) a

solve :: [(Integer, Integer)] -> Integer
solve ids = crt (map (uncurry (-)) ids) (product ids') ids'
  where
    ids' = map fst ids

solution :: IO ()
solution = do
  [_, line] <- readInput "src/Advent/Y2020/Day13/input"
  print $ solve $ parseIdList line
