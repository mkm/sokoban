module Grid where

import Data.Array.IArray (Array, array)
import Data.List (find)
import Data.Maybe (fromJust)

data Tile = Empty | Solid | Object | FreeTarget | OccupiedTarget
          deriving (Show, Read, Eq)

type Pos = (Int, Int)

data Grid = Grid (Array Pos Tile) Pos 
          deriving (Show, Read, Eq)

charToTile '.' = Empty
charToTile '#' = Solid
charToTile '*' = Object
charToTile 'o' = FreeTarget
charToTile '¤' = OccupiedTarget
charToTile '$' = Empty --Player

fromString src = Grid matrix playerPos
    where
      srcLines = lines src
      rowCount = length srcLines
      colCount = length $ head srcLines
      charPairs = zip [(x, y) | y <- [1 .. rowCount], x <- [1 .. colCount]] $ concat srcLines
      playerPos = fst $ fromJust $ find (('$' ==) . snd) charPairs
      tilePairs = map (\(i, c) -> (i, charToTile c)) charPairs
      matrix = array ((1, 1), (colCount, rowCount)) tilePairs

