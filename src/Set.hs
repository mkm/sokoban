module Set where

import Prelude hiding (elem)

data (Ord a) => Set a = Set [a]
                        deriving (Show)

instance (Ord a) => Eq (Set a) where
    a == b = a `subset` b && b `subset` a
--    Set a == Set b = a == b

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

subset :: (Ord a) => Set a -> Set a -> Bool
(Set xs) `subset` b = all (`elem` b) xs