{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.TrieBag
  ( TrieBag
  , lookup
  , delete
  , complete
  , insert
  , fromList
  , filter
  ) where

import qualified Data.Map        as Map
import           Data.Monoid
import           Data.Trie       (Trie (..))
import qualified Data.Trie       as Trie
import           Prelude         hiding (lookup, filter)
import           Test.QuickCheck

data TrieBag a where TrieBag :: Trie a (Sum Int) -> TrieBag [a]

deriving instance Eq a => Eq (TrieBag [a])
deriving instance Ord a => Ord (TrieBag [a])

instance (Arbitrary a, Ord a) => Arbitrary (TrieBag [a]) where
  arbitrary = fmap fromList' arbitrary where
    fromList' :: (Ord a) => [[a]] -> TrieBag [a]
    fromList' = fromList

instance Ord a => Monoid (TrieBag [a]) where
  mempty = TrieBag mempty
  mappend (TrieBag x) (TrieBag y) = TrieBag (mappend x y)

lookup :: (Foldable f, Ord a) => f a -> TrieBag [a] -> Int
lookup xs (TrieBag t) = getSum (Trie.lookup xs t)

delete :: (Foldable f, Ord a) => f a -> TrieBag [a] -> TrieBag [a]
delete xs (TrieBag t) = TrieBag (Trie.delete ((0>=).getSum) xs t)

complete :: (Foldable f, Ord a) => f a -> TrieBag [a] -> TrieBag [a]
complete xs (TrieBag t) = TrieBag (Trie.complete xs t)

insert :: (Foldable f, Ord a) => f a -> TrieBag [a] -> TrieBag [a]
insert xs (TrieBag t) = TrieBag (Trie.insert xs (Sum 1) t)

instance Foldable TrieBag where
  foldr f b (TrieBag t) = Trie.foldrWithKey (\k -> rep (f k) . getSum) b t where
    rep h = go where
      go 0 x = x
      go n x = h (go (n-1) x)
  foldMap f (TrieBag t) = Trie.foldMapWithKey rep t where
    rep _ (Sum 0) = mempty
    rep k (Sum r) = go m r where
      m = f k
      go a 1 = a
      go a n = go (m <> a) (n-1)
  length (TrieBag s) = size' s where
    size' (Trie (Sum e) c) = e + r where
      r = Map.foldl' (\a t -> a + size' t) 0 c

-- | prop> \xs (Blind p) -> (filter p . fromList) (xs :: [String]) === fromList [ x | x <- xs, p x ]
filter :: Ord a => ([a] -> Bool) -> TrieBag [a] -> TrieBag [a]
filter p (TrieBag t) = TrieBag (Trie.filterWithKey ((0<).getSum) p t)

instance Show a => Show (TrieBag [a]) where show = show . foldr (:) []

fromList :: (Foldable f, Foldable g, Ord a) => f (g a) -> TrieBag [a]
fromList = foldr insert mempty
