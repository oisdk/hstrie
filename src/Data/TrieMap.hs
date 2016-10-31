{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TrieMap
  ( TrieMap
  , lookup
  , delete
  , insert
  , fromList
  , assocs
  ) where

import           Prelude         hiding (lookup)

import           Data.Trie       (Trie (..))
import qualified Data.Trie       as Trie

newtype TrieMap f a b = TrieMap
  { getTrieMap :: Trie a (f b)
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Monoid)

lookup :: (Ord a, Monoid (f b)) => [a] -> TrieMap f a b -> f b
lookup xs (TrieMap t) = Trie.lookup xs t

delete :: (Ord a, Foldable f, Monoid (f b)) => [a] -> TrieMap f a b -> TrieMap f a b
delete xs (TrieMap t) = TrieMap (Trie.delete null xs t)

-- | prop> \t k v1 v2 -> insert (k :: String) (v2 :: Int) t === insert k v2 (insert k v1 t)
insert :: (Ord a, Monoid (f b), Applicative f) => [a] -> b -> TrieMap f a b -> TrieMap f a b
insert xs v (TrieMap t) = TrieMap (Trie.insert xs (pure v) t)

fromList :: (Foldable f, Ord a, Monoid (f b), Applicative f) => f ([a], b) -> TrieMap f a b
fromList = foldr (uncurry insert) mempty
{-# INLINE fromList #-}

assocs :: Foldable f => TrieMap f a b -> [([a],b)]
assocs = Trie.assocs . getTrieMap

-- | >>> fromList [("a",1),("b",2)]
-- fromList [("a",1),("b",2)]
instance (Show a, Show b, Foldable f) => Show (TrieMap f a b) where
  show = ("fromList " ++) . show . assocs
