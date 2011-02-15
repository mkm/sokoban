module Grid where

import Prelude hiding (Left, Right)
import Data.Array.IArray (Array, array, assocs, elems, inRange, bounds , (!), (//))
import Data.List (find, groupBy, sortBy)
import Data.Maybe (fromJust, mapMaybe, isJust)

import Generic
import Graph
import Set

data Tile = Solid | Empty Bool | Target Bool
            deriving (Show, Eq)

type Pos = (Int, Int)
type PosDelta = (Int, Int)

data Grid = Grid (Array Pos Tile) Pos 
            deriving (Show, Eq)

data StateInfo = StateInfo (Set Pos) Pos
                 deriving (Show, Eq)

data Direction = Up | Left | Down | Right
                 deriving (Show, Enum, Bounded)

charToTile :: Char -> Tile
charToTile '#' = Solid
charToTile '.' = Empty False
charToTile '*' = Empty True
charToTile 'o' = Target False
charToTile '¤' = Target True
charToTile '$' = Empty False --Player

tileToChar :: Tile -> Char
tileToChar Solid = '#'
tileToChar (Empty False) = '.'
tileToChar (Empty True) = '*'
tileToChar (Target False) = 'o'
tileToChar (Target True) = '¤'

fromString :: String -> Grid
fromString src = Grid gridArray playerPos
    where
      srcLines = lines src
      rowCount = length srcLines
      colCount = length $ head srcLines
      charPairs = zip [(x, y) | y <- [1 .. rowCount], x <- [1 .. colCount]] $ concat srcLines
      playerPos = fst $ fromJust $ find (('$' ==) . snd) charPairs
      tilePairs = Prelude.map (\(i, c) -> (i, charToTile c)) charPairs
      gridArray = array ((1, 1), (colCount, rowCount)) tilePairs

toString :: Grid -> String
toString (Grid gridArray pos) = unlines chars
    where
      tilePairs = assocs gridArray
      charPairs = Prelude.map (\(i, t) -> (i, tileToChar t)) tilePairs
      charPairsWithPlayer = applyWhere ((pos ==) . fst) (\(pos', _) -> (pos', '$')) charPairs
      srcLines = findGroups (\((_, y1), _) ((_, y2), _) -> y1 == y2) charPairsWithPlayer
      chars = Prelude.map (Prelude.map snd) $ sortBy (\(((_, y1), _):_) (((_, y2), _):_) -> compare y1 y2) srcLines

getStateInfo :: Grid -> StateInfo
getStateInfo (Grid a p) = StateInfo (Set.fromList boxPositions) p
    where
      assList = assocs a
      boxPositions = mapMaybe boxFilter assList
          where
            boxFilter (pos, Empty True) = Just pos
            boxFilter (pos, Target True) = Just pos
            boxFilter _ = Nothing

isAccepting :: Grid -> Bool
isAccepting (Grid gridArray _) = all hasBox $ filter isTarget $ elems gridArray

toGraph :: Grid -> Graph StateInfo Direction
toGraph grid = buildGraph grid startGraph startId
    where
      (startGraph, startId) = Graph.new $ getStateInfo grid
      buildGraph grid graph currentId = foldr updateGraph graph newGrids
          where
            newGrids = nextGrids grid
            updateGraph :: (Direction, Grid) -> Graph StateInfo Direction -> Graph StateInfo Direction
            updateGraph (dir, grid) graph =
                case getId stateInfo graph of
                  Nothing ->
                      let (newGraph, newStateId) = insertState stateInfo accepting graph
                          newGraph' = insertTransition dir (currentId, newStateId) newGraph
                      in buildGraph grid newGraph' newStateId 
                  Just id -> insertTransition dir (currentId, id) graph
                where
                  stateInfo = getStateInfo grid
                  accepting = Grid.isAccepting grid

nextGrids :: Grid -> [(Direction, Grid)]
nextGrids grid = mapMaybe (\dir -> move dir grid >>= \x -> return (dir, x)) [minBound .. maxBound] -- try all directions

move :: Direction -> Grid -> Maybe Grid
move dir (Grid gridArray pos) = do
  let newPos = posAdd pos $ dirDelta dir
  tile <- getTile newPos gridArray
  case tile of
    Solid -> Nothing
    _ | hasBox tile ->
          do
            newGridArray <- moveBox newPos gridArray
            Just $ Grid newGridArray newPos
      | otherwise -> Just $ Grid gridArray newPos
    where
      moveBox pos gridArray =
          do
            let newPos = posAdd pos $ dirDelta dir
            tile <- getTile newPos gridArray
            case tile of
              Solid -> Nothing
              _ | hasBox tile -> Nothing
                | otherwise -> Just $ toggleBox newPos $ toggleBox pos gridArray
  
toggleBox :: Pos -> Array Pos Tile -> Array Pos Tile
toggleBox pos gridArray =
    case getTile pos gridArray of
      Nothing -> gridArray
      Just tile -> gridArray // [(pos, newTile tile)]
    where
      newTile Solid = Solid
      newTile (Empty x) = Empty (not x)
      newTile (Target x) = Target (not x)

hasBox :: Tile -> Bool
hasBox Solid = False
hasBox (Empty x) = x
hasBox (Target x) = x

isTarget :: Tile -> Bool
isTarget (Target _) = True
isTarget _ = False

isLegal :: Direction -> Grid -> Bool
isLegal dir grid = isJust $ move dir grid 

getTile :: Pos -> Array Pos Tile -> Maybe Tile
getTile pos gridArray
        | inRange (bounds gridArray) pos = Just $ gridArray ! pos
        | otherwise = Nothing

dirDelta :: Direction -> PosDelta
dirDelta Up = (0, -1)
dirDelta Left = (-1, 0)
dirDelta Down = (0, 1)
dirDelta Right = (1, 0)

posAdd :: Pos -> PosDelta -> Pos
posAdd (x, y) (dx, dy) = (x + dx, y + dy)

