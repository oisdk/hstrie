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
  , filter
  ) where

import           Data.Trie       (Trie (..))
import qualified Data.Trie       as Trie
import           Prelude         hiding (lookup, filter)
import           Test.QuickCheck

newtype First a = First
  { getFirst :: Maybe a
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Arbitrary)

instance Monoid (First a) where
  mempty = First Nothing
  First Nothing `mappend` r = r
  l `mappend` _ = l

newtype TrieMap a b = TrieMap
  { getTrieMap :: Trie a (First b)
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Monoid, Arbitrary)

lookup :: (Foldable f, Ord a) => f a -> TrieMap a b -> Maybe b
lookup xs (TrieMap t) = getFirst (Trie.lookup xs t)

delete :: (Foldable f, Ord a) => f a -> TrieMap a b -> TrieMap a b
delete xs (TrieMap t) = TrieMap (Trie.delete isEmpty xs t) where
  isEmpty (First Nothing) = True
  isEmpty _ = False

complete :: (Foldable f, Ord a) => f a -> TrieMap a b -> TrieMap a b
complete xs (TrieMap t) = TrieMap (Trie.complete xs t)

-- |
-- prop> \t k v1 v2 -> insert (k :: String) (v2 :: Int) t === insert k v2 (insert k v1 t)
insert :: (Foldable f, Ord a) => f a -> b -> TrieMap a b -> TrieMap a b
insert xs v (TrieMap t) = TrieMap (Trie.insert xs (First (Just v)) t)

fromList :: (Foldable f, Foldable g, Ord a) => f (g a, b) -> TrieMap a b
fromList = foldr (uncurry insert) mempty
{-# INLINE fromList #-}

assocs :: TrieMap a b -> [([a],b)]
assocs = Trie.assocs . getTrieMap

-- |
-- prop> \xs (Blind p) -> fromList [ (k :: String,v :: Int) | (k,v) <- assocs xs, p v ] === filter p xs
filter :: Ord a => (b -> Bool) -> TrieMap a b -> TrieMap a b
filter p (TrieMap t) = TrieMap (Trie.mapMaybe f t) where
  f (First (Just x)) | p x = Just (First (Just x))
  f _ = Nothing

instance (Show a, Show b) => Show (TrieMap a b) where
  show = ("fromList" ++) . show . assocs
