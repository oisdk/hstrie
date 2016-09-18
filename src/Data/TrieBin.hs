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
  , filter
  ) where

import           Data.Trie       (Trie (..))
import qualified Data.Trie       as Trie
import           Prelude         hiding (lookup, filter)
import qualified Prelude as P
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

-- | prop> \xs (Blind p) -> (filter p . fromList) xs === (fromList . P.filter (p.snd)) (xs :: [(String,Int)])
filter :: Ord a => (b -> Bool) -> TrieBin a b -> TrieBin a b
filter p (TrieBin t) = TrieBin (Trie.mapMaybe (nie . P.filter p) t) where
  nie [] = Nothing
  nie xs = Just xs

instance (Show a, Show b) => Show (TrieBin a b) where
  show = ("fromList" ++) . show . assocs
