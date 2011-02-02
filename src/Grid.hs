module Grid where

import Prelude hiding (Left, Right)
import Data.Array.IArray (Array, array, assocs, inRange, bounds , (!))
import Data.List (find)
import Data.Maybe (fromJust, mapMaybe)

import Graph
import Set

data Tile = Empty | Solid | Box | FreeTarget | OccupiedTarget
          deriving (Show, Read, Eq)

type Pos = (Int, Int)
type PosDelta = (Int, Int)

data Grid = Grid (Array Pos Tile) Pos 
          deriving (Show, Read, Eq)

data StateInfo = StateInfo (Set Pos) Pos

charToTile :: Char -> Tile
charToTile '.' = Empty
charToTile '#' = Solid
charToTile '*' = Box
charToTile 'o' = FreeTarget
charToTile '¤' = OccupiedTarget
charToTile '$' = Empty --Player

fromString :: String -> Grid
fromString src = Grid gridArray playerPos
    where
      srcLines = lines src
      rowCount = length srcLines
      colCount = length $ head srcLines
      charPairs = zip [(x, y) | y <- [1 .. rowCount], x <- [1 .. colCount]] $ concat srcLines
      playerPos = fst $ fromJust $ find (('$' ==) . snd) charPairs
      tilePairs = map (\(i, c) -> (i, charToTile c)) charPairs
      gridArray = array ((1, 1), (colCount, rowCount)) tilePairs

getStateInfo :: Grid -> StateInfo
getStateInfo (Grid a p) = StateInfo (Set.fromList boxPositions) p
    where
      assList = assocs a
      boxPositions = mapMaybe boxFilter assList
          where
            boxFilter (pos, Box) = Just pos
            boxFilter (pos, OccupiedTarget) = Just pos
            boxFilter _ = Nothing

toGraph :: Grid -> Graph StateInfo
toGraph grid = buildGraph grid $ Graph.new $ getStateInfo grid
    where
      buildGraph grid graph = undefined

nextGrids :: Grid -> Set Grid
nextGrids grid = undefined

move :: Direction -> Grid -> Maybe Grid
move dir grid = undefined      

isLegal :: Direction -> Grid -> Bool
isLegal dir (Grid gridArray pos) = undefined -- gridArray ! newPos 
    where
      newPos = posAdd pos $ dirDelta dir

getTile :: Pos -> Array Pos Tile -> Maybe Tile
getTile pos gridArray
        | inRange (bounds gridArray) pos = Just $ gridArray ! pos

dirDelta :: Direction -> PosDelta
dirDelta Up = (0, -1)
dirDelta Left = (-1, 0)
dirDelta Down = (0, 1)
dirDelta Right = (1, 0)

posAdd :: Pos -> PosDelta -> Pos
posAdd (x, y) (dx, dy) = (x + dx, y + dy)