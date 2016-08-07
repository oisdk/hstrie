{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Trie where

import           Control.Monad
import           Data.Foldable
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Monoid
import           Prelude         hiding (lookup)
import           Test.QuickCheck

data Trie a b = Trie
  { endHere  :: b
  , children :: Map a (Trie a b)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- |
-- prop> \xs ys zs -> (xs :: Trie Char String) <> (ys <> zs) === (xs <> ys) <> zs
-- prop> \xs -> xs <> mempty === (xs :: Trie Char String)
-- prop> \xs -> mempty <> xs === (xs :: Trie Char String)
instance (Monoid b, Ord a) => Monoid (Trie a b) where
  mempty = Trie mempty Map.empty
  mappend (Trie a c) (Trie b d) = Trie (a <> b) (Map.unionWith (<>) c d)

lookup :: (Foldable f, Ord a, Monoid b) => f a -> Trie a b -> b
lookup = foldr f endHere where
  f e a = foldMap a . Map.lookup e . children

-- | Inserts a value into the Trie, mappending it with the previous, if
-- a previous exists.
insert :: (Foldable f, Ord a, Monoid b) => f a -> b -> Trie a b -> Trie a b
insert xs v = foldr f b xs where
  b (Trie p c) = Trie (p <> v) c
  f e a (Trie n c) = Trie n (Map.alter (Just . a . fold) e c)

instance (Arbitrary a, Ord a, Arbitrary b, Monoid b)
  => Arbitrary (Trie a b) where
    arbitrary = fmap fromList' arbitrary where
      fromList' :: (Ord a, Monoid b) => [([a],b)] -> Trie a b
      fromList' = foldr (uncurry insert) mempty

complete :: (Foldable f, Ord a, Monoid b) => f a -> Trie a b -> Trie a b
complete = foldr f id where
  f e a = foldMap a . Map.lookup e . children

prefixedBy :: (Foldable f, Ord a, Monoid b)
           => f a -> Trie a b -> Trie a b
prefixedBy xs = fold . foldr f Just xs where
  f e a =
    fmap (Trie mempty . Map.singleton e) . a <=< Map.lookup e . children

delete :: (Ord a, Foldable f, Monoid b)
       => (b -> Bool) -> f a -> Trie a b -> Trie a b
delete isEmpty xs = fold . foldr f flipEnd xs where
  f e a (Trie n c) = nilIfEmpty isEmpty (Trie n (Map.alter (a=<<) e c))

filterWithKey :: (Ord a, Monoid b)
              => (b -> Bool) -> ([a] -> Bool) -> Trie a b -> Trie a b
filterWithKey isFull = (fold .) . f where
  f p (Trie n c)
    | not (isFull n) = Trie n <$> tn
    | p [] = Just (Trie n t)
    | otherwise = Trie mempty <$> tn
    where
      t = Map.mapMaybeWithKey (f . (p .) . (:)) c
      tn = if Map.null t then Nothing else Just t

nilIfEmpty :: (b -> Bool) -> Trie a b -> Maybe (Trie a b)
nilIfEmpty isEmpty (Trie e c)
  | isEmpty e && Map.null c = Nothing
  | otherwise = Just (Trie e c)

flipEnd :: Monoid b => Trie a b -> Maybe (Trie a b)
flipEnd (Trie _ m)
  | Map.null m = Nothing
  | otherwise = Just (Trie mempty m)

foldrWithKey :: ([a] -> b -> c -> c) -> c -> Trie a b -> c
foldrWithKey f b (Trie e c) = f [] e r where
  r = Map.foldrWithKey ff b c
  ff k = flip (foldrWithKey $ f . (k:))

foldMapWithKey :: Monoid m => ([a] -> b -> m) -> Trie a b -> m
foldMapWithKey f (Trie e c) = s $ Map.foldMapWithKey ff c where
  s = mappend (f [] e)
  ff k = foldMapWithKey $ f . (k :)

assocs :: Foldable f => Trie a (f b) -> [([a], b)]
assocs = traverse toList <=< foldrWithKey (\k v a -> (k,v):a) []
