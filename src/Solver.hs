module Solver where

import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Control.Monad (liftM)
import Grid
import Graph
import Set

solve :: Grid -> Maybe [Direction]
solve grid = findPath (getStartingState graph) Set.empty
    where
      graph = toGraph grid
      findPath stateId pathStates
          | Graph.isAccepting stateId graph = Just []
          | otherwise =
              case subResults of
                [] -> Nothing
                _ -> Just $ minimumBy (\a b -> compare (length a) (length b)) subResults
          where
            nextTransitions = transitions stateId graph
            nextValidTransitions = filter (\(id, _) -> not $ Set.elem id pathStates) nextTransitions
            newPathStates = Set.insert stateId pathStates
            subResults = mapMaybe (\(id, dir) -> liftM (dir :) $ findPath id newPathStates) nextValidTransitions
            