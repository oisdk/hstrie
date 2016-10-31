{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

module Data.Trie
  ( Trie(..)
  , lookup
  , insert
  , add
  , fromList
  , fromAssocs
  , delete
  , foldrWithKey
  , foldMapWithKey
  , assocs
  , TrieMap
  , TrieBin
  ) where

import           Prelude         hiding (lookup)

import           Control.Monad   ((<=<))

import           Data.Foldable

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector

import           Data.Monoid     (First, (<>))
import           Data.Semiring

import           Test.QuickCheck (Arbitrary (..), Gen)

data Trie a b
  = Empty
  | Node b (Map a (Trie a b)) (Vector a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type TrieMap a b = Trie a (First b)
type TrieBin a b = Trie a [b]

instance (Ord a, Monoid b) => Monoid (Trie a b) where
  mempty = Empty
  mappend Empty t = t
  mappend t Empty = t
  mappend (Node s m xp) (Node t n yp) = followSplit xp (toList yp) . curry $ \case
    (Just (x,xs),y:ys) -> Node mempty (Map.fromList [(x, Node s m xs), (y, Node t n (Vector.fromList ys))])
    (Nothing    ,y:ys) -> Node  s     (Map.insertWith mappend y (Node t n (Vector.fromList ys)) m)
    (Just (x,xs),[]  ) -> Node     t  (Map.insertWith mappend x (Node s m xs) n)
    (Nothing    ,[]  ) -> Node (s<>t) (Map.unionWith mappend m n)

lookup :: (Ord a, Monoid b) => [a] -> Trie a b -> b
lookup _ Empty = mempty
lookup xs (Node e m p) = flip foldMap (stripPrefix p xs) $ \case
  [] -> e
  y:ys -> foldMap (lookup ys) (Map.lookup y m)

-- | Inserts a value into the Trie, mappending it with the previous, if
-- a previous exists. The previous value is put to the right-hand-side
-- of the mappend.
insert :: (Ord a, Monoid b) => [a] -> b -> Trie a b -> Trie a b
insert k v = go k where
  go xs Empty = Node v Map.empty (Vector.fromList xs)
  go xs (Node e m p) = followSplit p xs . curry $ \case
    (Nothing    ,[]  ) -> Node (v<>e) m
    (Nothing    ,z:zs) -> Node e      (Map.alter (Just . maybe (singleton (Vector.fromList zs) v) (go zs)) z m)
    (Just (y,ys),z:zs) -> Node mempty (Map.fromList [(y, Node e m ys), (z, singleton (Vector.fromList zs) v)])
    (Just (y,ys),[]  ) -> Node v      (Map.singleton y (Node e m ys))

instance (Ord a, Arbitrary a, Monoid b, Arbitrary b) => Arbitrary (Trie a b) where
  arbitrary = fmap fromAssocs (arbitrary :: (Arbitrary a, Arbitrary b) => Gen [([a], b)])

singleton :: Vector a -> b -> Trie a b
singleton xs v = Node v Map.empty xs

add :: (Ord a, Semiring b, Monoid b) => [a] -> Trie a b -> Trie a b
add xs = insert xs one

fromList :: (Ord a, Semiring b, Monoid b, Foldable f) => f [a] -> Trie a b
fromList = foldr add Empty

fromAssocs :: (Ord a, Monoid b, Foldable f) => f ([a], b) -> Trie a b
fromAssocs = foldr (uncurry insert) Empty

delete :: (Ord a, Monoid b) => (b -> Bool) -> [a] -> Trie a b -> Trie a b
delete p = (fold .) . go where
  compact = \case
    Empty -> Nothing
    Node e m xs | not (p e) -> case Map.toList m of
      [] -> Nothing
      [(y, Node f n ys)] -> Just (Node f n (Vector.concat [xs, Vector.singleton y, ys]))
      _ -> Just (Node e m xs)
    n -> Just n
  go _ Empty = Nothing
  go xs (Node e m xp) = followSplit xp xs . curry $ \case
    (Nothing,[]  ) -> compact . Node mempty m
    (Nothing,z:zs) -> compact . Node e (Map.alter (go zs =<<) z m)
    (Just _ ,_   ) -> Just . Node e m

foldrWithKey :: ([a] -> b -> c -> c) -> c -> Trie a b -> c
foldrWithKey _ b Empty = b
foldrWithKey f b (Node e c ks) = g [] e r where
  g = f . (vs++)
  vs = Vector.toList ks
  r = Map.foldrWithKey ff b c
  ff k = flip (foldrWithKey (g . (k:)))

foldMapWithKey :: Monoid m => ([a] -> b -> m) -> Trie a b -> m
foldMapWithKey _ Empty = mempty
foldMapWithKey f (Node e c ks) = s $ Map.foldMapWithKey ff c where
  g = f . (vs++)
  vs = Vector.toList ks
  s = mappend (g [] e)
  ff k = foldMapWithKey $ g . (k :)

followSplit :: Eq a => Vector a -> [a] -> (Maybe (a, Vector a) -> [a] -> Vector a -> c) -> c
followSplit xv yl c = Vector.ifoldr f b xv yl where
  b ys = c Nothing ys xv
  f _ x a (y:ys) | x == y = a ys
  f i x _ ys = c (Just (x, Vector.drop (i+1) xv) ) ys (Vector.take i xv)

stripPrefix :: Eq a => Vector a -> [a] -> Maybe [a]
stripPrefix = foldr f Just where
  f x a (y:ys) | x == y = a ys
  f _ _ _ = Nothing

assocs :: Foldable f => Trie a (f b) -> [([a], b)]
assocs = traverse toList <=< foldrWithKey (\k v a -> (k,v):a) []
