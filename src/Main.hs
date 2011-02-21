module Main where

import Prelude hiding (Left, Right)
import System (getArgs)
import Grid
import Graph
import Solver

main = do
  (fileName:_) <- getArgs
  input <- readFile fileName
  let grid = Grid.fromString input
  print $ newSolve grid
  loop grid

loop grid = do
  printGrid grid
  case Grid.isAccepting grid of
    False ->
        do
          dir <- getAction
          case move dir grid of
            Nothing -> loop grid
            Just newGrid -> loop newGrid
    True -> win

win = putStrLn "You have won"

getAction = do
  cmd <- getLine
  case cmd of
    "w" -> return Up
    "a" -> return Left
    "s" -> return Down
    "d" -> return Right
    _ -> getAction

printGrid = putStrLn . Grid.toString
