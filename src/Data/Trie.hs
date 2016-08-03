{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Trie
  ( TrieBin
  , TrieBag
  , TrieMap
  , TrieSet
  , lookup
  , member
  , insert
  , delete
  , assocs
  , complete
  , fromList
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Coerce
import           Data.Foldable
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Monoid hiding (Last(..))
import           Prelude             hiding (lookup)
import           Test.QuickCheck

data Trie_ a b = Trie_
  { endHere_  :: b
  , children_ :: Map a (Trie_ a b)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Ord a, Monoid b) => Monoid (Trie_ a b) where
  mempty = Trie_ mempty Map.empty
  mappend (Trie_ a m) (Trie_ b n) = Trie_ (mappend a b) (Map.unionWith mappend m n)

lookup_ :: (Foldable f, Ord a, Monoid b) => f a -> Trie_ a b -> b
lookup_ = foldr f endHere_ where
  f e a = foldMap a . Map.lookup e . children_

complete_ :: (Foldable f, Ord a, Monoid b) => f a -> Trie_ a b -> Trie_ a b
complete_ = foldr f id where
  f e a = foldMap a . Map.lookup e . children_

insert_ :: (Foldable f, Ord a, Monoid b) => f a -> b -> Trie_ a b -> Trie_ a b
insert_ xs v = foldr f b xs where
  b (Trie_ p c) = Trie_ (mappend v p) c
  f e a (Trie_ n c) = Trie_ n (Map.alter (Just . a . fromMaybe mempty) e c)

delete_ :: (Ord a, Foldable f, Val b) => f a -> Trie_ a b -> Trie_ a b
delete_ xs = fold . foldr f b xs where
  b (Trie_ _ m) | Map.null m = Nothing
                | otherwise = Just (Trie_ mempty m)
  f e a (Trie_ n c) = nilIfEmpty (Trie_ n (Map.alter (a=<<) e c))
  nilIfEmpty (Trie_ e c) | isEmpty e && null c = Nothing
                         | otherwise = Just (Trie_ e c)

foldrWithKey_ :: ([a] -> b -> c -> c) -> c -> Trie_ a b -> c
foldrWithKey_ f b (Trie_ e c) = Map.foldrWithKey ff s c where
  s = f [] e b
  ff k = flip (foldrWithKey_ $ f . (k:))

foldMapWithKey_ :: Monoid m => ([a] -> b -> m) -> Trie_ a b -> m
foldMapWithKey_ f (Trie_ e c) = s $ Map.foldMapWithKey ff c where
  s = mappend (f [] e)
  ff k = foldMapWithKey_ $ f . (k :)

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

data TrieSet a where TrieSet :: Trie_ a Any -> TrieSet [a]
data TrieBag a where TrieBag :: Trie_ a (Sum Int) -> TrieBag [a]

newtype Last a = Last
  { getLast :: Maybe a
  } deriving (Functor, Foldable, Traversable, Eq, Ord)

instance Monoid (Last a) where
  mempty = Last Nothing
  mappend l (Last Nothing) = l
  mappend _ r = r

newtype TrieMap a b = TrieMap
  { _getTrieMap :: Trie_ a (Last b)
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

newtype TrieBin a b = TrieBin
  { _getTrieBin :: Trie_ a [b]
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Foldable TrieSet where
  foldr f b (TrieSet t) = foldrWithKey_ (\k -> bool (f k) id . getAny) b t
  foldMap f (TrieSet t) = foldMapWithKey_ (\k -> bool (f k) mempty . getAny) t
  length (TrieSet s) = size' s where
    size' (Trie_ (Any e) c) = if e then 1 + r else r where
      r = Map.foldl' (\a t -> a + size' t) 0 c

instance Foldable TrieBag where
  foldr f b (TrieBag t) = foldrWithKey_ (\k -> rep (f k) . getSum) b t where
    rep h = go where
      go 0 x = x
      go n x = go (n-1) (h x)
  foldMap f (TrieBag t) = foldMapWithKey_ (\k -> rep (f k) . getSum) t where
    rep x = go where
      go 0 = mempty
      go n = mappend x (go (n-1))
  length (TrieBag s) = size' s where
    size' (Trie_ (Sum e) c) = e + r where
      r = Map.foldl' (\a t -> a + size' t) 0 c

class Monoid t => Trie t where
  type Key t :: *
  type ValRep t :: *
  toRep :: t -> Trie_ (Key t) (ValRep t)
  fromRep :: Trie_ (Key t) (ValRep t) -> t

class Monoid m => Val m where
  type MayVal m :: *
  type SureVal m :: *
  repToMay :: m -> MayVal m
  sureToRep :: SureVal m -> m
  repToList :: m -> [SureVal m]
  isEmpty :: m -> Bool

instance Val Any where
  type MayVal Any = Bool
  type SureVal Any = Bool
  repToMay = getAny
  sureToRep = Any
  repToList (Any False) = []
  repToList (Any True) = [True]
  isEmpty (Any b) = not b

instance Val (Sum Int) where
  type MayVal (Sum Int) = Int
  type SureVal (Sum Int) = Int
  repToMay = getSum
  sureToRep = Sum
  repToList (Sum n) = replicate n 1
  isEmpty (Sum n) = n <= 0

instance Val [a] where
  type SureVal [a] = a
  type MayVal [a] = [a]
  repToMay = id
  sureToRep = pure
  repToList = id
  isEmpty = null

instance Val (Last b) where
  type SureVal (Last b) = b
  type MayVal (Last b) = Maybe b
  repToMay = getLast
  sureToRep = Last . Just
  repToList = toList
  isEmpty = null

instance Ord a => Trie (TrieSet [a]) where
  type Key (TrieSet [a]) = a
  type ValRep (TrieSet [a]) = Any
  toRep (TrieSet t) = t
  fromRep = TrieSet

instance Ord a => Trie (TrieBag [a]) where
  type Key (TrieBag [a]) = a
  type ValRep (TrieBag [a]) = Sum Int
  toRep (TrieBag t) = t
  fromRep = TrieBag

instance Ord a => Trie (TrieMap a b) where
  type Key (TrieMap a b) = a
  type ValRep (TrieMap a b) = Last b
  toRep = coerce
  fromRep = coerce

instance Ord a => Trie (TrieBin a b) where
  type Key (TrieBin a b) = a
  type ValRep (TrieBin a b) = [b]
  toRep = coerce
  fromRep = coerce

lookup :: (Trie t, Foldable f, Ord (Key t), Val (ValRep t)) => f (Key t) -> t -> MayVal (ValRep t)
lookup xs = repToMay . lookup_ xs . toRep

member :: (Trie t, Foldable f, Ord (Key t), Val (ValRep t)) => f (Key t) -> t -> Bool
member xs = not . isEmpty . lookup_ xs . toRep

insert :: (Trie t, Foldable f, Ord (Key t), Val (ValRep t)) => f (Key t) -> SureVal (ValRep t) -> t -> t
insert xs vs | isEmpty v = id
             | otherwise = fromRep . insert_ xs v . toRep
             where v = sureToRep vs

fromList :: (Foldable f, Foldable g, Trie t, Ord (Key t), Val (ValRep t)) => f (g (Key t), SureVal (ValRep t)) -> t
fromList = foldr (uncurry insert) mempty

fromList' :: (Trie t, Ord (Key t), Val (ValRep t)) => [([Key t], SureVal (ValRep t))] -> t
fromList' = foldr (uncurry insert) mempty

delete :: (Trie t, Foldable f, Ord (Key t), Val (ValRep t)) => f (Key t) -> t -> t
delete xs = fromRep . delete_ xs . toRep

assocs :: (Trie t, Val (ValRep t)) => t -> [([Key t],SureVal (ValRep t))]
assocs = traverse repToList <=< foldrWithKey_ (\k v a -> (k,v):a) [] . toRep

complete :: (Trie t, Val (ValRep t), Foldable f, Ord (Key t)) => f (Key t) -> t -> t
complete xs = fromRep . complete_ xs . toRep

instance Show a => Show (TrieSet [a]) where
  show = show . toList

instance Ord a => Monoid (TrieSet [a]) where
  mempty = TrieSet mempty
  mappend (TrieSet x) (TrieSet y) = TrieSet (mappend x y)

instance (Ord a, Arbitrary a) => Arbitrary (TrieSet [a]) where
  arbitrary = fmap fromList' arbitrary

instance Show a => Show (TrieBag [a]) where
  show = show . toList

instance Ord a => Monoid (TrieBag [a]) where
  mempty = TrieBag mempty
  mappend (TrieBag x) (TrieBag y) = TrieBag (mappend x y)

instance (Ord a, Arbitrary a) => Arbitrary (TrieBag [a]) where
  arbitrary = fmap (fromList' . (map.fmap) abs) arbitrary

instance (Show a, Show b, Ord a) => Show (TrieBin a b) where
  show = ("fromList "++) . show . assocs

instance Ord a => Monoid (TrieBin a b) where
  mempty = TrieBin mempty
  mappend (TrieBin x) (TrieBin y) = TrieBin (mappend x y)

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (TrieBin a b) where
  arbitrary = fmap fromList' arbitrary

instance (Show a, Show b, Ord a) => Show (TrieMap a b) where
  show = ("fromList "++) . show . assocs

instance Ord a => Monoid (TrieMap a b) where
  mempty = TrieMap mempty
  mappend (TrieMap x) (TrieMap y) = TrieMap (mappend x y)

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (TrieMap a b) where
  arbitrary = fmap fromList' arbitrary
