module Tree where

import Data.Map as Map hiding (update)

type Tree a b = Map.Map a b

data (Ord a) => OldTree a b = Leaf | Branch (OldTree a b) a b (OldTree a b)
                           deriving (Show)

empty = Map.empty

_empty :: (Ord a) => OldTree a b
_empty = Leaf


insertOrModify k v f = alter g k
    where
      g Nothing = Just v
      g (Just v) = Just (f v)

_insertOrModify :: (Ord a) => a -> b -> (b -> b) -> OldTree a b -> OldTree a b
_insertOrModify k v f = fst . update k updater
    where
      updater Nothing = (Just v, ())
      updater (Just v) = (Just (f v), ())

lookup a b = Map.lookup a b

_lookup :: (Ord a) => a -> OldTree a b -> Maybe b
_lookup k = snd . update k updater
    where
      updater x = (x, x)

update :: (Ord a) => a -> (Maybe b -> (Maybe b, c)) -> OldTree a b -> (OldTree a b, c)
update k f Leaf =
    case f Nothing of
      (Nothing, x) -> (Leaf, x)
      (Just v, x) -> (Branch Leaf k v Leaf, x)
update k1 f (Branch left k2 v right) =
    case compare k1 k2 of
      LT -> let (left', x) = update k1 f left
            in (Branch left' k2 v right, x)
      EQ -> case f (Just v) of
              (Nothing, x) -> (mergeBranches left right, x)
              (Just v', x) -> (Branch left k2 v' right, x)
      GT -> let (right', x) = update k1 f right
            in (Branch left k2 v right', x)

mergeBranches :: (Ord a) => OldTree a b -> OldTree a b -> OldTree a b
mergeBranches Leaf r = r
mergeBranches l Leaf = l
mergeBranches (Branch ll lK lV Leaf) r = Branch ll lK lV r
mergeBranches (Branch ll lK lV (Branch lrl lrK lrV lrr)) r = Branch (Branch ll lK lV (mergeBranches lrl lrr)) lrK lrV r

