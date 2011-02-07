module Graph where

newtype Id = Id Int
    deriving (Enum)

data State a = State Id a

data Transition b = Transition (Id, Id) b

type StartingStateId = Id
type AcceptingStateId = Id

data Graph a b = Graph [State a] [Transition b] StartingStateId [AcceptingStateId] [Id]

new :: a -> (Graph a b, Id)
new x = (Graph [State (Id 1) x] [] (Id 1) [] [(Id 2) ..], Id 1)

getStateData :: State a -> a
getStateData (State _ x) = x

getStateId :: State a -> Id
getStateId (State id _) = id

getStates :: Graph a b -> [State a]
getStates (Graph states _ _ _ _) = states

containsData :: (Eq a) => a -> Graph a b -> Bool
containsData x = any ((x ==) . getStateData) . getStates

insertState :: a -> Graph a b -> (Graph a b, Id)
insertState x (Graph states transitions startingId acceptingIds (id:ids)) =
    (Graph (State id x : states) transitions startingId acceptingIds ids, id)

insertTransition :: b -> (Id, Id) -> Graph a b -> Graph a b
insertTransition x idPair (Graph states transitions startingId acceptingIds ids) =
    Graph states (Transition idPair x : transitions) startingId acceptingIds ids