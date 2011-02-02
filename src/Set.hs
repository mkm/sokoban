module Set where

import Prelude hiding (elem)

data (Ord a) => Set a = Set [a]

empty :: (Ord a) => Set a
empty = Set []

insert :: (Ord a) => a -> Set a -> Set a
insert e (Set []) = Set [e]
insert e (Set (x:xs)) =
    case compare e x of
      LT -> Set (e : x : xs)
      EQ -> Set (x : xs)
      GT -> Set (x : ys)
          where (Set ys) = insert e (Set xs)

elem :: (Ord a) => a -> Set a -> Bool
elem e (Set []) = False
elem e (Set (x:xs))
     | e == x = True
     | otherwise = elem e (Set xs)

fromList :: (Ord a) => [a] -> Set a
fromList = foldr insert empty

