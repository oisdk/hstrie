{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.TrieSet
  ( TrieSet
  , member
  , delete
  , complete
  , insert
  , fromList
  ) where

import qualified Data.Map        as Map
import           Data.Monoid
import           Data.Trie       (Trie (..))
import qualified Data.Trie       as Trie
import           Test.QuickCheck


data TrieSet a where TrieSet :: Trie a Any -> TrieSet [a]

deriving instance Eq a => Eq (TrieSet [a])
deriving instance Ord a => Ord (TrieSet [a])

instance Ord a => Monoid (TrieSet [a]) where
  mempty = TrieSet mempty
  mappend (TrieSet x) (TrieSet y) = TrieSet (mappend x y)

instance (Ord a, Arbitrary a) => Arbitrary (TrieSet [a]) where
  arbitrary = fmap fromList' arbitrary where
    fromList' :: Ord a => [[a]] -> TrieSet [a]
    fromList' = fromList

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

instance Foldable TrieSet where
  foldr f b (TrieSet t) = Trie.foldrWithKey (\k -> bool (f k) id . getAny) b t
  foldMap f (TrieSet t) = Trie.foldMapWithKey (\k -> bool (f k) mempty . getAny) t
  length (TrieSet s) = size' s where
    size' (Trie (Any e) c) = if e then 1 + r else r where
      r = Map.foldl' (\a t -> a + size' t) 0 c

member :: (Foldable f, Ord a) => f a -> TrieSet [a] -> Bool
member xs (TrieSet t) = getAny (Trie.lookup xs t)

delete :: (Foldable f, Ord a) => f a -> TrieSet [a] -> TrieSet [a]
delete xs (TrieSet t) = TrieSet (Trie.delete (not.getAny) xs t)

complete :: (Foldable f, Ord a) => f a -> TrieSet [a] -> TrieSet [a]
complete xs (TrieSet t) = TrieSet (Trie.complete xs t)

insert :: (Foldable f, Ord a) => f a -> TrieSet [a] -> TrieSet [a]
insert xs (TrieSet t) = TrieSet (Trie.insert xs (Any True) t)

instance Show a => Show (TrieSet [a]) where show = show . foldr (:) []

fromList :: (Foldable f, Foldable g, Ord a) => f (g a) -> TrieSet [a]
fromList = foldr insert mempty
