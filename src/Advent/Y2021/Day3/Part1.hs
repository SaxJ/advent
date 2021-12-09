{-# LANGUAGE OverloadedStrings #-}
module Advent.Y2021.Day3.Part1 where
import Advent.Input ( readInput )
import Relude.Unsafe (read)

solution :: IO ()
solution = do
  lines <- readInput "src/Advent/Y2021/Day3/input.text"
  print "hello"

gamma :: [String] -> Integer
