module Set where

import Prelude hiding (elem)
import Data.Maybe (isJust)
import Tree

data (Ord a) => Set a = Set (Tree a a)
    deriving (Show)

instance (Ord a) => Eq (Set a) where
    Set a == Set b = Tree.toList a == Tree.toList b

empty :: (Ord a) => Set a
empty = Set Tree.empty

singleton :: (Ord a) => a -> Set a
singleton e = Set.insert e Set.empty

insert :: (Ord a) => a -> Set a -> Set a
insert e (Set tree) = Set $ insertOrModify e e id tree

elem :: (Ord a) => a -> Set a -> Bool
elem e (Set tree) = isJust $ Tree.lookup e tree

fromList :: (Ord a) => [a] -> Set a
fromList = foldr insert Set.empty

toList :: (Ord a) => Set a -> [a]
toList (Set tree) = Tree.toValueList tree

map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
map f (Set tree) = Set $ Tree.map (\(k, _) -> let k' = f k in (k', k')) tree