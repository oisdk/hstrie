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
  , prefixedBy
  , filter
  ) where

import           Data.Foldable
import qualified Data.Map        as Map
import           Data.Monoid
import           Data.Trie       (Trie (..))
import qualified Data.Trie       as Trie
import           Prelude         hiding (filter)
import           Test.QuickCheck

data TrieSet a where TrieSet :: Trie a Any -> TrieSet [a]

deriving instance Eq a => Eq (TrieSet [a])
deriving instance Ord a => Ord (TrieSet [a])

instance Ord a => Monoid (TrieSet [a]) where
  mempty = TrieSet mempty
  mappend (TrieSet x) (TrieSet y) = TrieSet (x <> y)

instance (Ord a, Arbitrary a) => Arbitrary (TrieSet [a]) where
  arbitrary = fmap fromList' arbitrary where
    fromList' :: Ord a => [[a]] -> TrieSet [a]
    fromList' = fromList

instance Foldable TrieSet where
  foldr f b (TrieSet t) =
    Trie.foldrWithKey (\k (Any e) -> if e then f k else id) b t
  foldMap f (TrieSet t) =
    Trie.foldMapWithKey (\k (Any e) -> if e then f k else mempty) t
  length (TrieSet s) = size' s where
    size' (Trie (Any e) c) = if e then 1 + r else r where
      r = Map.foldl' (\a t -> a + size' t) 0 c

-- | prop> \xs -> all (`member ` fromList (xs :: [String])) (take 5 xs)
member :: (Foldable f, Ord a) => f a -> TrieSet [a] -> Bool
member xs (TrieSet t) = getAny (Trie.lookup xs t)
{-# INLINE member #-}

-- | prop> \xs -> conjoin [ (delete s . fromList) xs === fromList [ x | x <- xs, s /= x ] | s <- take 5 xs :: [String] ]
delete :: (Foldable f, Ord a) => f a -> TrieSet [a] -> TrieSet [a]
delete xs (TrieSet t) = TrieSet (Trie.delete (not.getAny) xs t)

complete :: (Foldable f, Ord a) => f a -> TrieSet [a] -> TrieSet [a]
complete xs (TrieSet t) = TrieSet (Trie.complete xs t)

prefixedBy :: (Foldable f, Ord a) => f a -> TrieSet [a] -> TrieSet [a]
prefixedBy xs (TrieSet t) = TrieSet (Trie.prefixedBy xs t)

insert :: (Foldable f, Ord a) => f a -> TrieSet [a] -> TrieSet [a]
insert xs (TrieSet t) = TrieSet (Trie.insert xs (Any True) t)
{-# INLINE insert #-}

-- | prop> \xs (Blind p) -> (filter p . fromList) (xs :: [String]) === fromList [ x | x <- xs, p x ]
filter :: Ord a => ([a] -> Bool) -> TrieSet [a] -> TrieSet [a]
filter p (TrieSet t) = TrieSet (Trie.filterWithKey getAny p t)

instance Show a => Show (TrieSet [a]) where show = show . foldr (:) []

fromList :: (Foldable f, Foldable g, Ord a) => f (g a) -> TrieSet [a]
fromList = foldr insert mempty
