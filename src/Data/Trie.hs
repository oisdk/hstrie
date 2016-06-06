{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Trie
  ( TrieSet
  , TrieMap
  , TrieBag
  , insert
  , lookup
  , completions
  , prefixedBy
  , showTrie
  ) where

import           Control.Lens    hiding (children)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           GHC.Exts        (IsList(..))
import           Prelude         hiding (lookup)
import           Test.QuickCheck
import           Control.Monad

data Trie a b = Trie
   { _trieEndHere  :: b
   , _trieChildren :: Map a (Trie a b)
   } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeFields ''Trie

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Trie a b) where
  arbitrary = sized arb where
    arb n
      | n <= 0 = flip Trie Map.empty <$> arbitrary
      | otherwise = do
          keys <- arbitrary
          list <- traverse (\key -> (,) key <$> arb m) keys
          frst <- arbitrary
          pure (Trie frst (Map.fromList list))
          where m = n `div` 5

data TrieSet a where TrieSet :: Trie a Bool -> TrieSet [a]

deriving instance Eq a => Eq (TrieSet [a])
deriving instance Ord a => Ord (TrieSet [a])

instance (Ord a, Arbitrary a) => Arbitrary (TrieSet [a]) where
  arbitrary = TrieSet <$> arbitrary

class (HasChildren a (Map keyEl (Trie keyEl end)), HasEndHere a end) => AsTrie a keyEl end where
  trie :: Iso' a (Trie keyEl end)

instance AsTrie (Trie a b) a b where trie = simple

instance HasEndHere (TrieSet [a]) Bool where endHere = trie.endHere
instance HasChildren (TrieSet [a]) (Map a (Trie a Bool)) where children = trie.children
instance AsTrie (TrieSet [a]) a Bool where trie = iso (\(TrieSet t) -> t) TrieSet

newtype TrieMap a b = TrieMap
  { _getTrieMap :: Trie a (Maybe b)
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Arbitrary)

instance HasEndHere (TrieMap a b) (Maybe b) where endHere = trie.endHere
instance HasChildren (TrieMap a b) (Map a (Trie a (Maybe b))) where children = trie.children
instance AsTrie (TrieMap a b) a (Maybe b) where trie = coerced

newtype TrieBag a f b = TrieBag
  { _getTrieBag :: Trie a (f b)
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Arbitrary)

instance HasEndHere (TrieBag a f b) (f b) where endHere = trie.endHere
instance HasChildren (TrieBag a f b) (Map a (Trie a (f b))) where children = trie.children
instance AsTrie (TrieBag a f b) a (f b) where trie = coerced

class Nullable a where isEmpty :: a -> Bool
instance Foldable f => Nullable (TrieBag a f b) where isEmpty = null
instance Nullable (TrieSet [a]) where isEmpty = null
instance Nullable (TrieMap a b) where isEmpty = null

nilIfEmpty :: Nullable a => a -> Maybe a
nilIfEmpty x = if isEmpty x then Nothing else Just x

foldrWithKey :: AsTrie t a b => ([a] -> b -> c -> c) -> c -> t -> c
foldrWithKey f i t = Map.foldrWithKey ff s (t^.children) where
  s = f [] (t^.endHere) i
  ff k = flip (foldrWithKey $ f . (k:))

foldMapWithKey :: (AsTrie t a b, Monoid m) => ([a] -> b -> m) -> t -> m
foldMapWithKey f t = s $ Map.foldMapWithKey ff (t^.children) where
  s = mappend (f [] (t^.endHere))
  ff k = foldMapWithKey $ f . (k :)

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

instance Foldable TrieSet where
  foldr f i (TrieSet t) = foldrWithKey (\k -> bool (f k) id) i t
  foldMap f (TrieSet t) = foldMapWithKey (\k -> bool (f k) mempty) t
  length (TrieSet s) = size' s where
    size' t = if t^.endHere then 1 + r else r where
      r = Map.foldl' (\a e -> a + size' e) 0 (t^.children)

union :: (Ord a, AsTrie t a b) => (b -> b -> b) -> t -> t -> t
union fb t = (children %~ Map.unionWith (union fb) (t^.children)) . (endHere %~ fb (t^.endHere))

instance Ord a => Monoid (TrieSet [a]) where
  mappend = union (||)
  mempty  = TrieSet (Trie False Map.empty)

instance Ord a => Monoid (TrieMap a b) where
  mappend = union (`maybe` Just)
  mempty  = TrieMap (Trie Nothing Map.empty)

instance (Ord a, Monoid (f b)) => Monoid (TrieBag a f b) where
  mappend = union mappend
  mempty = TrieBag (Trie mempty Map.empty)

insert :: (Ord a, Foldable f, AsTrie t a b, Monoid t) => f a -> b -> t -> t
insert = insert' mempty where
  insert' :: (Ord a, Foldable f, AsTrie t a b) => t -> f a -> b -> t -> t
  insert' m xs x = over trie $ foldr f (endHere .~ x) xs where
    f e a = over (children . at e) (Just . a . fromMaybe m')
    m' = m ^. trie

instance Ord a => IsList (TrieSet [a]) where
  type Item (TrieSet [a]) = [a]
  fromList = foldr (`insert` True) mempty
  toList = foldr (:) []

instance Ord a => IsList (TrieMap a b) where
  type Item (TrieMap a b) = ([a],b)
  fromList = foldr (\(xs,v) -> insert xs (Just v)) mempty
  toList = foldrWithKey (\k v a -> maybe a (\y -> (k,y) : a) v) []

instance (Ord a, Monoid (f b), Foldable f) => IsList (TrieBag a f b) where
  type Item (TrieBag a f b) = ([a], f b)
  fromList = foldr (uncurry insert) mempty
  toList = foldrWithKey (\k v a -> if null v then a else (k,v) : a) []

lookup :: (AsTrie t a b, Foldable f, Ord a) => b -> f a -> t -> b
lookup x = views trie . foldr f (view endHere) where
  f e = views (children . at e) . maybe x

instance (Ord a, Show a) => Show (TrieSet [a]) where
  show = ("fromList " ++) . show . toList

instance (Ord a, Show a, Show b) => Show (TrieMap a b) where
  show = ("fromList " ++) . show . toList

instance (Ord a, Show a, Show (f b), Monoid (f b), Foldable f) => Show (TrieBag a f b) where
  show = ("fromList " ++) . show . toList

prefixedBy :: (Ord a, Foldable f, AsTrie t a b, Monoid t) => f a -> t -> t
prefixedBy = begins' mempty where
  begins' :: (Ord a, Foldable f, AsTrie t a b) => t -> f a -> t -> t
  begins' m xs = trie %~ fromMaybe m' . foldr f Just xs where
    f e a t = do
      child <- view (children . at e) t
      m' & (children . at e . traverse) (const $ a child)
    m' = m ^. trie

showTrie :: AsTrie t a b => (a -> Char) -> (b -> Char) -> t -> String
showTrie a c = views trie (unlines . showTrie') where
  showTrie' = views children (ff <=< Map.assocs)
  ff (k,t) = zipWith (++) pads $ case showTrie' t of
    [] -> [[]]
    r -> r
    where pads = [a k, views endHere c t] : repeat "  "

completions :: (AsTrie t a b, Ord a, Foldable s, Nullable t, Monoid t)
      => s a -> Lens' t t
completions = alter' nilIfEmpty mempty where
  alter' :: (Foldable s, AsTrie t a b, Functor f, Ord a)
         => (t -> Maybe t) -> t -> s a -> (t -> f t) -> t -> f t
  alter' nie empt xs i = trie (foldr f (from trie i) xs) where
    f e a = (children . at e) (fmap nie' . a . orEmpt)
    nie' = from trie nie
    orEmpt = fromMaybe (view trie empt)

type instance Index (TrieMap a b) = [a]
type instance IxValue (TrieMap a b) = b
instance Ord a => Ixed (TrieMap a b) where ix i = completions i . endHere . traverse

type instance Index (TrieBag a f b) = [a]
type instance IxValue (TrieBag a f b) = f b
instance (Traversable f, Ord a, Monoid (f b)) => Ixed (TrieBag a f b) where
  ix i = completions i . endHere

type instance Index (TrieSet [a]) = [a]
type instance IxValue (TrieSet [a]) = Bool
instance Ord a => Ixed (TrieSet [a]) where
  ix i = completions i . endHere

instance Ord a => Contains (TrieSet [a]) where
  contains i = completions i . endHere

instance Ord a => At (TrieMap a b) where at i = completions i . endHere

instance (Traversable f, Ord a, Monoid (f b)) => At (TrieBag a f b) where
  at i f = completions i . endHere $ (fmap (fromMaybe mempty) . f . nie) where
    nie t = if null t then Nothing else Just t
