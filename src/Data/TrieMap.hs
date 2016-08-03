{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TrieMap
  ( TrieMap
  , lookup
  , delete
  , complete
  , insert
  , fromList
  , assocs
  ) where

import           Data.Trie       (Trie (..))
import qualified Data.Trie       as Trie
import           Prelude         hiding (lookup)
import           Test.QuickCheck

newtype Last a = Last
  { getLast :: Maybe a
  } deriving (Eq, Ord, Read, Show, Functor, Traversable, Arbitrary)

instance Monoid (Last a) where
        mempty = Last Nothing
        l `mappend` Last Nothing = l
        _ `mappend` r            = r

instance Foldable Last where
    foldMap f = foldMap f . getLast

newtype TrieMap a b = TrieMap
  { getTrieMap :: Trie a (Last b)
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Monoid, Arbitrary)

lookup :: (Foldable f, Ord a) => f a -> TrieMap a b -> Maybe b
lookup xs (TrieMap t) = getLast (Trie.lookup xs t)

delete :: (Foldable f, Ord a) => f a -> TrieMap a b -> TrieMap a b
delete xs (TrieMap t) = TrieMap (Trie.delete isEmpty xs t) where
  isEmpty (Last Nothing) = True
  isEmpty _ = False

complete :: (Foldable f, Ord a) => f a -> TrieMap a b -> TrieMap a b
complete xs (TrieMap t) = TrieMap (Trie.complete xs t)

insert :: (Foldable f, Ord a) => f a -> b -> TrieMap a b -> TrieMap a b
insert xs v (TrieMap t) = TrieMap (Trie.insert xs (Last (Just v)) t)

fromList :: (Foldable f, Foldable g, Ord a) => f (g a, b) -> TrieMap a b
fromList = foldr (uncurry insert) mempty

assocs :: TrieMap a b -> [([a],b)]
assocs = Trie.assocs . getTrieMap

instance (Show a, Show b) => Show (TrieMap a b) where
  show = ("fromList" ++) . show . assocs
