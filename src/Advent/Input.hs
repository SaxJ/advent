module Advent.Input where

import Data.Either (rights)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read (decimal)
import Data.Text.Internal.Read (digitToInt)

-- | Parse each line of a file into a list
readInput ::
  -- | A file path
  Text ->
  IO [Text]
readInput filename = T.lines <$> T.readFile (T.unpack filename)

readIntegers :: Text -> IO [Integer]
readIntegers fileName = do
  lines <- readInput fileName
  let reads = map decimal lines
  return $ map fst $ rights reads

bintodec :: String -> Integer
bintodec str = sum powers
  where
    powers = zipWith (curry pwr) [0..] (reverse str)
    pwr (i, c)
            | c == '0' = 0
            | otherwise = 2 ^ i
