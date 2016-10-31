{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.TrieSet
  ( TrieSet
  , TrieBag
  , member
  , add
  , fromList
  , delete
  , suffixes
  ) where

import           Prelude         hiding (filter)

import           Data.Monoid
import           Data.Semigroup  (stimesMonoid)
import           Data.Semiring

import           Data.Trie       (Trie (..))
import qualified Data.Trie       as Trie

import           Test.QuickCheck (Arbitrary (..), Gen)

data TrieSet b a where TrieSet :: Trie a (Add b) -> TrieSet b [a]

type TrieBag = TrieSet Int

deriving instance (Eq a, Eq b) => Eq (TrieSet b [a])
deriving instance (Ord a, Ord b) => Ord (TrieSet b [a])

suffixes :: (Ord a, Semiring b) => [a] -> TrieSet b [a] -> TrieSet b [a]
suffixes xs (TrieSet t) = TrieSet (Trie.suffixes xs t)

instance (Ord a, Semiring b) => Monoid (TrieSet b [a]) where
  mempty = TrieSet Empty
  mappend (TrieSet x) (TrieSet y) = TrieSet (x <> y)

instance Enum b => Foldable (TrieSet b) where
  foldr f b (TrieSet t) =
    Trie.foldrWithKey (\k (Add e) -> rep (fromEnum e) (f k)) b t
  foldMap f (TrieSet t) =
    Trie.foldMapWithKey (\k (Add e) -> stimesMonoid (fromEnum e) (f k)) t
  length (TrieSet t) = (getAdd . foldMap (fmap fromEnum)) t
  null (TrieSet Empty) = True
  null _ = False

member :: (Ord a, Semiring b) => [a] -> TrieSet b [a] -> b
member xs (TrieSet t) = getAdd (Trie.lookup xs t)

delete :: (Ord a, Semiring b, Enum b) => [a] -> TrieSet b [a] -> TrieSet b [a]
delete xs (TrieSet t) = TrieSet (Trie.delete ((0<).fromEnum.getAdd) xs t)

add :: (Ord a, Semiring b) => [a] -> TrieSet b [a] -> TrieSet b [a]
add xs (TrieSet t) = TrieSet (Trie.add xs t)

instance (Show a, Enum b) => Show (TrieSet b [a]) where show = show . foldr (:) []

fromList :: (Foldable f, Ord a, Semiring b) => f [a] -> TrieSet b [a]
fromList = foldr add mempty

instance (Arbitrary a, Semiring b, Ord a) => Arbitrary (TrieSet b [a]) where
  arbitrary = fmap fromList (arbitrary :: Arbitrary a => Gen [a])

rep :: Int -> (a -> a) -> a -> a
rep m f x = go m where
  go 0 = x
  go n = f (go (n-1))
