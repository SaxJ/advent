{-# LANGUAGE OverloadedStrings #-}
module Advent.Y2021.Day4.Part1 where
import Advent.Input (readInput )
import Data.List.Split (splitOn, chunksOf)
import Relude.Unsafe (read)
import Data.Maybe (fromJust)
import qualified Data.Matrix as M
import Data.String (words)
import Prelude hiding (words)
import Data.List (elemIndex)

matrixDimensions = (5, 5)

falseMatrix = M.fromList 5 5 $ repeat False

matrixOp :: M.Matrix Integer -> Integer -> M.Matrix Bool
matrixOp mtx val = case idx of
  Just i -> let
    r = i `div` M.ncols mtx
    c = i `mod` M.nrows mtx
    in M.setElem True (r, c) falseMatrix
  _ -> falseMatrix
  where
    idx = elemIndex val $ M.toList mtx

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
