{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Advent.Y2020.Day11.Part1 where

import Advent.Input
import Data.List (foldl, (!!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (unpack, Text)
import Data.Map (Map)

type Grid = Map (Int, Int) Char

makeMap :: [Text] -> Grid
makeMap lines = Map.fromList $ concatMap transform lineNums
  where
    lineNums = zip [0 ..] lines
    transform (n, ln) = zipWith (\ cn c -> ((cn, n), c)) [0 ..] (unpack ln)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x -1, y), (x + 1, y), (x, y + 1), (x, y -1), (x + 1, y + 1), (x -1, y + 1), (x -1, y -1), (x + 1, y -1)]

noOccupiedNeighbours :: Grid -> (Int, Int) -> Bool
noOccupiedNeighbours grid coord = Just '#' `notElem` map gridLookup (neighbours coord)
  where
    gridLookup coord = Map.lookup coord grid

lotsOfNeighbours :: Grid -> (Int, Int) -> Bool
lotsOfNeighbours grid coord = length occupiedNeighs >= 4
  where
    gridLookup coord = Map.lookup coord grid
    occupiedNeighs = filter (== Just '#') $ map gridLookup (neighbours coord)

nextState :: Grid -> (Int, Int) -> Char
nextState grid coord
  | curr == Just '.' = '.'
  | curr == Just 'L' && noOccupiedNeighbours grid coord = '#'
  | curr == Just '#' && lotsOfNeighbours grid coord = 'L'
  | otherwise = fromJust curr
  where
    curr = Map.lookup coord grid

nextGrid :: Grid -> Grid
nextGrid grid = Map.fromList $ map stateMapper $ Map.toList grid
  where
    stateMapper (coord, _) = (coord, nextState grid coord)

gridGenerations grid = foldl (\ls@((_, last) : _) generation -> (generation, nextGrid last) : ls) [(0, grid)]

converge :: (a -> a -> Bool) -> [a] -> a
converge pred (x : y : ls)
  | pred x y = y
  | otherwise = converge pred (y : ls)

gridSteps = iterate nextGrid

finalGridState :: Grid -> Grid
finalGridState = converge mapConvergence . gridSteps
  where
    mapConvergence ma mb = Map.null $ Map.differenceWith mapDiff ma mb

countOccupied :: Grid -> Int
countOccupied = Map.size . Map.filter (== '#')

mapDiff :: Char -> Char -> Maybe Char
mapDiff a b
  | a == b = Nothing
  | otherwise = Just a

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2020/Day11/input"
  let initial = makeMap lines
  print $ countOccupied $ finalGridState initial
