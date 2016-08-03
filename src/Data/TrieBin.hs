{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TrieBin
  ( TrieBin
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

newtype TrieBin a b = TrieBin
  { getTrieBin :: Trie a [b]
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Monoid, Arbitrary)

lookup :: (Foldable f, Ord a) => f a -> TrieBin a b -> [b]
lookup xs (TrieBin t) = Trie.lookup xs t

delete :: (Foldable f, Ord a) => f a -> TrieBin a b -> TrieBin a b
delete xs (TrieBin t) = TrieBin (Trie.delete null xs t)

complete :: (Foldable f, Ord a) => f a -> TrieBin a b -> TrieBin a b
complete xs (TrieBin t) = TrieBin (Trie.complete xs t)

insert :: (Foldable f, Ord a) => f a -> b -> TrieBin a b -> TrieBin a b
insert xs v (TrieBin t) = TrieBin (Trie.insert xs [v] t)

fromList :: (Foldable f, Foldable g, Ord a) => f (g a, b) -> TrieBin a b
fromList = foldr (uncurry insert) mempty

assocs :: TrieBin a b -> [([a],b)]
assocs = Trie.assocs . getTrieBin

instance (Show a, Show b) => Show (TrieBin a b) where
  show = ("fromList" ++) . show . assocs
