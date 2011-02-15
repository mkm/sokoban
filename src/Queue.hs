module Queue where

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

insert :: a -> Queue a -> Queue a
insert e (Queue xs ys) = Queue xs (e : ys)

remove :: Queue a -> Maybe (a, Queue a)
remove (Queue [] []) = Nothing
remove (Queue [] ys) = remove $ Queue (reverse ys) []
remove (Queue (x:xs) ys) = Just (x, Queue xs ys)