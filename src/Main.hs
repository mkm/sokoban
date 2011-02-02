module Main where

import System (getArgs)
import Grid

main = do
  (fileName:_) <- getArgs
  input <- readFile fileName
  print $ fromString input
