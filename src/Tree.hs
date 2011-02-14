module Tree where

import Prelude hiding (lookup)

data (Ord a) => Tree a b = Leaf | Branch !(Tree a b) a b !(Tree a b)
                           deriving (Show)

empty :: (Ord a) => Tree a b
empty = Leaf

isEmpty :: (Ord a) => Tree a b -> Bool
isEmpty Leaf = True
isEmpty _ = False

insertOrModify :: (Ord a) => a -> b -> (b -> b) -> Tree a b -> Tree a b
insertOrModify k v f =  update k updater
    where
      updater Nothing = Just v
      updater (Just v) = Just $ f v

lookup :: (Ord a) => a -> Tree a b -> Maybe b
lookup _ Leaf = Nothing
lookup k1 (Branch l k2 v r) =
    case compare k1 k2 of
      LT -> lookup k1 l
      EQ -> Just v
      GT -> lookup k1 r

update :: (Ord a) => a -> (Maybe b -> Maybe b) -> Tree a b -> Tree a b
update k f Leaf =
    case f Nothing of
      Nothing -> Leaf
      Just v -> Branch Leaf k v Leaf
update k1 f (Branch left k2 v right) =
    case compare k1 k2 of
      LT -> Branch (update k1 f left) k2 v right
      EQ -> case f (Just v) of
              Nothing -> mergeBranches left right
              Just v' -> Branch left k2 v' right
      GT -> Branch left k2 v (update k1 f right)

mergeBranches :: (Ord a) => Tree a b -> Tree a b -> Tree a b
mergeBranches Leaf r = r
mergeBranches l Leaf = l
mergeBranches (Branch ll lK lV Leaf) r = Branch ll lK lV r
mergeBranches (Branch ll lK lV (Branch lrl lrK lrV lrr)) r = Branch (Branch ll lK lV (mergeBranches lrl lrr)) lrK lrV r

toList :: (Ord a) => Tree a b -> [(a, b)]
toList Leaf = []
toList (Branch l k v r) = toList l ++ [(k, v)] ++ toList r

toValueList :: (Ord a) => Tree a b -> [b]
toValueList = map snd . toList