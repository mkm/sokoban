module Graph where

import Data.List (find)
import Tree
import Set

newtype Id = Id Int
    deriving (Enum, Show, Eq, Ord)

data State a = State Id a
               deriving (Show)

data Transition b = Transition Id b
                    deriving (Show)

instance Eq (Transition b) where
    (Transition id1 _) == (Transition id2 _) = id1 == id2

instance Ord (Transition b) where
    compare (Transition id1 _) (Transition id2 _) = compare id1 id2 

type StartingStateId = Id
type AcceptingStateId = Id

data Graph a b = Graph [State a] (Tree Id (Set (Transition b))) StartingStateId [AcceptingStateId] [Id]

instance (Show a, Show b) => Show (Graph a b) where
    show (Graph states transitions startingId acceptingIds (availableId:_)) =
        "States:\n" ++ show states ++
        "\nTransitions:\n" ++ show transitions ++
        "\nStarting id:\n" ++ show startingId ++
        "\nAccepting ids:\n" ++ show acceptingIds ++
        "\nFirst available id:\n" ++ show availableId ++
        "\n"

new :: a -> (Graph a b, Id)
new x = (Graph [State (Id 1) x] Tree.empty (Id 1) [] [(Id 2) ..], Id 1)

getStateData :: State a -> a
getStateData (State _ x) = x

getStateId :: State a -> Id
getStateId (State id _) = id

getStates :: Graph a b -> [State a]
getStates (Graph states _ _ _ _) = states

getTransitions :: Graph a b -> Tree Id (Set (Transition b))
getTransitions (Graph _ transitions _ _ _) = transitions

getId :: (Eq a) => a -> Graph a b -> Maybe Id
getId x graph = (find ((x ==) . getStateData) . getStates) graph >>= return . getStateId

insertState :: a -> Bool -> Graph a b -> (Graph a b, Id)
insertState x accepting (Graph states transitions startingId acceptingIds (id:ids)) =
    (Graph (State id x : states) transitions startingId acceptingIds' ids, id)
    where
      acceptingIds' = if accepting then id : acceptingIds else acceptingIds

insertTransition :: b -> (Id, Id) -> Graph a b -> Graph a b
insertTransition x (src, dst) (Graph states transitions startingId acceptingIds ids) =
    Graph states (Tree.insertOrModify src (Set.singleton transition) (Set.insert transition) transitions) startingId acceptingIds ids
    where
      transition = Transition dst x

isAccepting :: Id -> Graph a b -> Bool
isAccepting id (Graph _ _ _ acceptingIds _) = Prelude.elem id acceptingIds

getStartingState :: Graph a b -> Id
getStartingState (Graph _ _ startingId _ _) = startingId

transitions :: Id -> Graph a b -> [(Id, b)]
transitions id graph =
    case Tree.lookup id (getTransitions graph) of
      Nothing -> []
      Just idSet -> map (\(Transition dst x) -> (dst, x)) $ Set.toList idSet