module Graph where

import Data.List (find)

newtype Id = Id Int
    deriving (Enum, Show)

data State a = State Id a
               deriving (Show)

data Transition b = Transition (Id, Id) b
                    deriving (Show)

type StartingStateId = Id
type AcceptingStateId = Id

data Graph a b = Graph [State a] [Transition b] StartingStateId [AcceptingStateId] [Id]

instance (Show a, Show b) => Show (Graph a b) where
    show (Graph states transitions startingId acceptingIds (availableId:_)) =
        "States:\n" ++ show states ++
        "\nTransitions:\n" ++ show transitions ++
        "\nStarting id:\n" ++ show startingId ++
        "\nAccepting ids:\n" ++ show acceptingIds ++
        "\nFirst available id:\n" ++ show availableId ++
        "\n"

new :: a -> (Graph a b, Id)
new x = (Graph [State (Id 1) x] [] (Id 1) [] [(Id 2) ..], Id 1)

getStateData :: State a -> a
getStateData (State _ x) = x

getStateId :: State a -> Id
getStateId (State id _) = id

getStates :: Graph a b -> [State a]
getStates (Graph states _ _ _ _) = states

getId :: (Eq a) => a -> Graph a b -> Maybe Id
getId x graph = (find ((x ==) . getStateData) . getStates) graph >>= return . getStateId

insertState :: a -> Bool -> Graph a b -> (Graph a b, Id)
insertState x accepting (Graph states transitions startingId acceptingIds (id:ids)) =
    (Graph (State id x : states) transitions startingId acceptingIds' ids, id)
    where
      acceptingIds' = if accepting then id : acceptingIds else acceptingIds

insertTransition :: b -> (Id, Id) -> Graph a b -> Graph a b
insertTransition x idPair (Graph states transitions startingId acceptingIds ids) =
    Graph states (Transition idPair x : transitions) startingId acceptingIds ids

