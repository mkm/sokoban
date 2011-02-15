module Solver where

import Data.Maybe (mapMaybe, fromJust)
import Data.List (minimumBy)
import Control.Monad (liftM)
import Grid
import Queue
import Relation
import Graph
import Set

newSolve :: Grid -> Maybe [Direction]
newSolve grid = undefined
    where
      graph = toGraph grid

      markStates queue graph =
          case Queue.remove queue of
            Nothing -> graph
            Just (id, queue') -> markStates queue'' graph'
                where
                  fromIds = transitionsTo id graph

                  mark (stateInfo, Nothing) = Just (stateInfo, Just $ 1 + (snd $ fromJust $ dataFromId id graph))
                  mark _ = Nothing

                  folder fromId (q, g) = (if modified then q' else q, g')
                      where
                        (modified, g') = modifyState fromId mark g
                        q' = Queue.insert fromId q

                  (queue'', graph') = undefined -- foldr folder (queue', graph) fromIds

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
            nextTransitions = Prelude.map (\(RelPair (a, b)) -> (a, b)) $ Set.toList $ transitionsFrom stateId graph
            nextValidTransitions = filter (\(id, _) -> not $ Set.elem id pathStates) nextTransitions
            newPathStates = Set.insert stateId pathStates
            subResults = mapMaybe (\(id, dir) -> liftM (dir :) $ findPath id newPathStates) nextValidTransitions
            