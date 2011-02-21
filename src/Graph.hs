module Graph where

import Data.List (find)
import Control.Monad (liftM)
import Relation
import Set

newtype Id = Id Int
    deriving (Enum, Show, Eq, Ord)

data State a = State Id a
               deriving (Show)

type StartingStateId = Id
type AcceptingStateId = Id

data Graph a b = Graph [State a] (Relation Id Id b) StartingStateId [AcceptingStateId] [Id]

instance (Show a, Show b) => Show (Graph a b) where
    show (Graph states transitions startingId acceptingIds (availableId:_)) =
        "States:\n" ++ show states ++
        "\nTransitions:\n" ++ show transitions ++
        "\nStarting id:\n" ++ show startingId ++
        "\nAccepting ids:\n" ++ show acceptingIds ++
        "\nFirst available id:\n" ++ show availableId ++
        "\n"

new :: a -> (Graph a b, Id)
new x = (Graph [State (Id 1) x] Relation.empty (Id 1) [] [(Id 2) ..], Id 1)

getStateData :: State a -> a
getStateData (State _ x) = x

getStateId :: State a -> Id
getStateId (State id _) = id

getStates :: Graph a b -> [State a]
getStates (Graph states _ _ _ _) = states

getTransitions :: Graph a b -> Relation Id Id b
getTransitions (Graph _ transitions _ _ _) = transitions

getStartingState :: Graph a b -> StartingStateId
getStartingState (Graph _ _ startingId _ _) = startingId

getAcceptingStates :: Graph a b -> [AcceptingStateId]
getAcceptingStates (Graph _ _ _ acceptingIds _) = acceptingIds

getId :: (Eq a) => a -> Graph a b -> Maybe Id
getId x graph = (find ((x ==) . getStateData) . getStates) graph >>= return . getStateId

insertState :: a -> Bool -> Graph a b -> (Graph a b, Id)
insertState x accepting (Graph states transitions startingId acceptingIds (id:ids)) =
    (Graph (State id x : states) transitions startingId acceptingIds' ids, id)
    where
      acceptingIds' = if accepting then id : acceptingIds else acceptingIds

insertTransition :: b -> (Id, Id) -> Graph a b -> Graph a b
insertTransition x (src, dst) (Graph states transitions startingId acceptingIds ids) =
    Graph states (Relation.insert src dst x transitions) startingId acceptingIds ids

isAccepting :: Id -> Graph a b -> Bool
isAccepting id (Graph _ _ _ acceptingIds _) = Prelude.elem id acceptingIds

transitionsFrom :: Id -> Graph a b -> Set (RelPair Id b)
transitionsFrom id graph = Relation.lookupFst id (getTransitions graph)

transitionsTo :: Id -> Graph a b -> Set (RelPair Id b)
transitionsTo id graph = Relation.lookupSnd id (getTransitions graph)

mapStates :: (a -> c) -> Graph a b -> Graph c b
mapStates f (Graph states transitions startingId acceptingIds ids) =
    Graph (Prelude.map (\(State id x) -> State id (f x)) states) transitions startingId acceptingIds ids

modifyState :: Id -> (a -> Maybe a) -> Graph a b -> (Bool, Graph a b)
modifyState id f (Graph states transitions startingId acceptingIds ids) =
    (modified, Graph states' transitions startingId acceptingIds ids)
    where
      modifyOccurence [] = Nothing
      modifyOccurence (state@(State id' x):states)
          | id == id' =
              case f x of
                Nothing -> Nothing
                Just x' -> Just $ (State id' x') : states
          | otherwise = liftM (state :) (modifyOccurence states)
      (modified, states') =
          case modifyOccurence states of
            Nothing -> (False, states)
            Just states' -> (True, states')

dataFromId :: Id -> Graph a b -> Maybe a
dataFromId id graph = liftM getStateData $ find ((id ==) . getStateId) (getStates graph)