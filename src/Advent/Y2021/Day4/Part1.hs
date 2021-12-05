{-# LANGUAGE OverloadedStrings #-}
module Advent.Y2021.Day4.Part1 where
import Advent.Input (readInput )
import Data.List.Split (splitOn, chunksOf)
import Relude.Unsafe (read)
import Data.Maybe (fromJust)
import qualified Data.Matrix as M
import Data.String (words)
import Prelude hiding (words)

matrixDimensions = (5, 5)

matricesFromInput :: [String] -> [M.Matrix Integer]
matricesFromInput = map makeMatrix . matrixChunks

makeMatrix :: [String] -> M.Matrix Integer
makeMatrix = M.fromList 5 5 . map read . concatMap words

matrixChunks :: [String] -> [[String]]
matrixChunks = chunksOf 5 . fromJust . viaNonEmpty tail

readCommands :: [String] -> [Integer]
readCommands = map read . splitOn "," . fromJust . viaNonEmpty head

getInputs :: [String] -> [String]
getInputs = filter (not . null)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day4/input.test"
  print "hello"
