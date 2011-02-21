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
newSolve grid = findPath properGraph
    where
      graph = mapStates (\stateInfo -> (stateInfo, Nothing)) $ toGraph grid

      acceptingIds = Queue.fromList $ Graph.getAcceptingStates graph

      markStates :: Queue Id -> Graph (StateInfo, Maybe Int) Direction -> Graph (StateInfo, Maybe Int) Direction
      markStates queue graph =
          case Queue.remove queue of
            Nothing -> graph
            Just (id, queue') -> markStates queue'' graph'
                where
                  fromIds = Prelude.map (fst . Relation.unpackRelPair) $ Set.toList $ transitionsTo id graph

                  mark :: (StateInfo, Maybe Int) -> Maybe (StateInfo, Maybe Int)
                  mark (stateInfo, Nothing) = Just (stateInfo, Just $ 1 + (fromJust $ snd $ fromJust $ dataFromId id graph))
                  mark _ = Nothing

                  folder :: Id -> (Queue Id, Graph (StateInfo, Maybe Int) Direction) -> (Queue Id, Graph (StateInfo, Maybe Int) Direction)
                  folder fromId (q, g) = (if modified then q' else q, g')
                      where
                        (modified, g') = modifyState fromId mark g
                        q' = Queue.insert fromId q

                  (queue'', graph') = foldr folder (queue', graph) fromIds

      properGraph = markStates acceptingIds graph

      findPath :: Graph (StateInfo, Maybe Int) Direction -> Maybe [Direction]
      findPath graph = search (Graph.getStartingState graph)
          where
            search :: Id -> Maybe [Direction]
            search current =
                if current `Prelude.elem` getAcceptingStates graph
                then Just []
                else
                    case bestDir of
                      Nothing -> Nothing
                      Just (id, dir) -> liftM (dir :) $ search id
                where
                  relPairs = Set.toList $ transitionsFrom current graph
                  
                  comparator (RelPair (id1, _)) (RelPair (id2, _)) =
                      case (snd $ fromJust $ dataFromId id1 graph, snd $ fromJust $ dataFromId id2 graph) of
                        (Nothing, Nothing) -> EQ
                        (Nothing, _) -> GT
                        (_, Nothing) -> LT
                        (Just x, Just y) -> compare x y

                  bestDir =
                      case relPairs of
                        [] -> Nothing
                        _ ->
                            case minimumBy comparator relPairs of
                              RelPair (id, dir) ->
                                  case (snd $ fromJust $ dataFromId id graph) of
                                    Nothing -> Nothing
                                    _ -> Just (id, dir)

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