{-# LANGUAGE OverloadedStrings #-}
module Advent.Y2021.Day4.Part2 where
import Advent.Input (readInput )
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Matrix as M
import Data.String (words)
import Prelude hiding (words)
import Data.List (elemIndex, foldl, findIndex, minimumBy, maximumBy)
import Debug.Trace
import Data.Text (unpack)
import Data.Function (on)

matrixDimensions = (5, 5)

falseMatrix = M.fromList 5 5 $ repeat False

-- Runs through commands accumulating operation matrices
accumulateOperations :: [Integer] -> M.Matrix Integer -> [M.Matrix Bool]
accumulateOperations cmds mtx = foldl accumulator [falseMatrix] cmds
  where
    accumulator ms cmd = ms ++ [next]
      where
        next = M.elementwise (||) (matrixOp mtx cmd) (last ms)


indexToCoord :: M.Matrix Integer -> Int -> (Int, Int)
indexToCoord m i = (i `div` M.ncols m + 1, i `mod` M.nrows m + 1)

matrixOp :: M.Matrix Integer -> Integer -> M.Matrix Bool
matrixOp mtx val = case midx of
  Just i -> let
    (r, c) = indexToCoord mtx i
    in M.setElem True (r, c) falseMatrix
  _ -> falseMatrix
  where
    midx = elemIndex val $ M.toList mtx

matricesFromInput :: [String] -> [M.Matrix Integer]
matricesFromInput = map makeMatrix . matrixChunks

makeMatrix :: [String] -> M.Matrix Integer
makeMatrix = M.fromList 5 5 . map read . concatMap words

matrixChunks :: [String] -> [[String]]
matrixChunks = chunksOf 5 . tail

readCommands :: [String] -> [Integer]
readCommands = map read . splitOn "," . head

getInputs :: [String] -> [String]
getInputs = filter (not . null)

checkMatrix :: M.Matrix Bool -> Bool
checkMatrix m = any and lists
  where
    rows = map (`M.getRow` m) [1..5]
    cols = map (`M.getCol` m) [1..5]
    lists = rows ++ cols

findSuccess :: [M.Matrix Bool] -> (Int, [M.Matrix Bool])
findSuccess ms = (idx, solved)
  where
    idx = fromMaybe 1000 $ findIndex checkMatrix ms
    solved = dropWhile (not . checkMatrix) ms

matrixScore :: M.Matrix Integer -> M.Matrix Bool -> Integer
matrixScore m bm = sum $ map fst $ filter (not . snd) zipped
  where
    zipped = zip (M.toList m) (M.toList bm)

solve lines = matrixScore (matrices !! i) (head solved) * (cmds !! (idx - 1))
  where
    inputs = getInputs lines
    cmds = readCommands inputs
    matrices = matricesFromInput inputs
    solveSteps = map (accumulateOperations cmds) matrices
    zs = zip [0..] $ map findSuccess solveSteps
    finalResult@(i, (idx, solved)) = maximumBy (compare `on` (fst.snd)) zs


solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day4/input"
  print $ solve $ map unpack lines
