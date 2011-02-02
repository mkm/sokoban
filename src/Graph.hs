module Graph where

newtype Id = Id Int
    deriving (Enum)

data State a = State Id a

data Direction = Up | Left | Down | Right

data Transition = Transition (Id, Id) Direction

type StartingStateId = Id
type AcceptingStateId = Id

data Graph a = Graph [State a] [Transition] StartingStateId [AcceptingStateId] [Id]

new :: a -> Graph a
new x = Graph [State (Id 1) x] [] (Id 1) [] [(Id 2) ..]