module Relation where

import Tree
import Set

newtype RelPair a b = RelPair (a, b)
    deriving (Show)

instance (Eq a) => Eq (RelPair a b) where
    RelPair (a1, _) == RelPair (a2, _) = a1 == a2

instance (Ord a) => Ord (RelPair a b) where
    compare (RelPair (a1, _)) (RelPair (a2, _)) = compare a1 a2

data Relation a b c = Relation (Tree a (Set (RelPair b c))) (Tree b (Set (RelPair a c)))
                      deriving (Show)

empty :: (Ord a, Ord b) => Relation a b c
empty = Relation Tree.empty Tree.empty

insert :: (Ord a, Ord b) => a -> b -> c -> Relation a b c -> Relation a b c
insert kFst kSnd v (Relation treeFst treeSnd) = Relation treeFst' treeSnd'
    where
      treeFst' = Tree.insertOrModify kFst (Set.singleton (RelPair (kSnd, v))) (Set.insert (RelPair (kSnd, v))) treeFst
      treeSnd' = Tree.insertOrModify kSnd (Set.singleton (RelPair (kFst, v))) (Set.insert (RelPair (kFst, v))) treeSnd

lookupFst :: (Ord a, Ord b) => a -> Relation a b c -> Set (RelPair b c)
lookupFst k (Relation tree _) = Tree.lookupDefault k Set.empty tree

lookupSnd :: (Ord a, Ord b) => b -> Relation a b c -> Set (RelPair a c)
lookupSnd k (Relation _ tree) = Tree.lookupDefault k Set.empty tree