{-# LANGUAGE UndecidableInstances  #-}

module Data.Trie.List.Set where

import qualified Data.Map.Strict  as Map
import           Data.Map.Strict     (Map)

import           Data.Bool           (bool)
import           Data.Maybe          (isJust)
import           Data.Foldable       (Foldable(..))
import           Data.Semigroup      (Semigroup(..),stimesIdempotent)

import           Control.Applicative (Applicative(..),liftA2)

import           Control.Lens hiding (children)

import           GHC.Exts            (IsList(Item))
import qualified GHC.Exts         as OverloadedLists

import           Data.Trie.Internal.Ap
import           Data.Coerce.Utilities

instance (Ord a, [a] ~ b) =>
         Semigroup (Trie b) where
    Trie x xs <> Trie y ys = Trie (x || y) (Map.unionWith (<>) xs ys)
    {-# INLINE (<>) #-}
    stimes = stimesIdempotent
    {-# INLINE stimes #-}

instance (Ord a, [a] ~ b) =>
         Monoid (Trie b) where
    mappend = (<>)
    {-# INLINE mappend #-}
    mempty = Trie False Map.empty
    {-# INLINE mempty #-}

data Trie a where
        Trie :: Bool -> Map a (Trie [a]) -> Trie [a]

deriving instance (Eq a, b ~ [a]) => Eq (Trie b)
deriving instance (Ord a, b ~ [a]) => Ord (Trie b)

endsHere :: Lens (Trie a) (Trie a) Bool Bool
endsHere f (Trie e m) = fmap (flip Trie m) (f e)
{-# INLINE endsHere #-}

children :: Lens (Trie [a]) (Trie [b]) (Map a (Trie [a])) (Map b (Trie [b]))
children f (Trie e m) = fmap (Trie e) (f m)
{-# INLINE children #-}

instance Foldable Trie where
    foldr f b (Trie e c)
      | e = f [] r
      | otherwise = r
      where
        r = Map.foldrWithKey (\x tr xs -> foldr (f . (:) x) xs tr) b c
    foldr' f !b (Trie e c)
      | e = f [] r
      | otherwise = r
      where
        !r = Map.foldrWithKey' (\x tr !xs -> foldr' (f . (:) x) xs tr) b c
    foldl f b (Trie e c) =
        Map.foldlWithKey (\xs x -> foldl (\a -> f a . (:) x) xs) (bool b (f b []) e) c
    foldl' f !b (Trie e c) =
        Map.foldlWithKey' (\xs x -> foldl' (\ !a -> f a . (:) x) xs) r c
      where
        !r = bool b (f b []) e
    foldMap f (Trie e c)
      | e = f [] `mappend` r
      | otherwise = r
      where
        r = Map.foldMapWithKey (\x -> foldMap (f . (:) x)) c
    length = go 0
      where
        go :: Int -> Trie a -> Int
        go !n (Trie False m) = Map.foldl' go n m
        go !n (Trie True m) = Map.foldl' go (n + 1) m

instance (Ord b, c1 ~ [b], c2 ~ [b], a1 ~ a2) =>
         Each (Trie a1) (Trie c1) a2 c2 where
    each f (Trie e c)
      | e = liftA2 insert (f []) r
      | otherwise = r
      where
        r = getAp (Map.foldMapWithKey (\x -> Ap #. each (f . (:) x)) c)
    {-# INLINE each #-}

instance (Show a, b ~ [a]) =>
         Show (Trie b) where
    showsPrec n = showsPrec n . toList

instance (Ord a, b ~ [a]) =>
         IsList (Trie b) where
    type Item (Trie b) = b
    fromList = fromList
    {-# INLINE fromList #-}
    toList = toList
    {-# INLINE toList #-}

insert
    :: (Ord a, Foldable f)
    => f a -> Trie [a] -> Trie [a]
insert =
    foldr
        (\x xs -> children . at x %~ Just . xs . fold)
        (endsHere .~ True)

delete
    :: (Ord a, Foldable f)
    => f a -> Trie [a] -> Trie [a]
delete =
    foldr
        (\x xs -> children . at x %~ (=<<) (nonEmpty . xs))
        (endsHere .~ False)

nonEmpty :: Trie a -> Maybe (Trie a)
nonEmpty tr@(Trie e m)
  | not e && Map.null m = Nothing
  | otherwise = Just tr

member :: (Ord a, Foldable f) => f a -> Trie [a] -> Bool
member = foldr (\x -> anyOf (children . ix x)) (view endsHere)

type instance Index (Trie a) = a
type instance IxValue (Trie a) = ()

instance (Ord a, [a] ~ b) =>
         Contains (Trie b) where
    contains xs k =
        foldr
            (\x a -> (children . at x) (fmap nonEmpty . a . fold))
            (endsHere k)
            xs
    {-# INLINE contains #-}

instance (Ord a, [a] ~ b) =>
         Ixed (Trie b) where
    ix xs f tr
      | member xs tr = tr <$ f ()
      | otherwise = pure tr
    {-# INLINE ix #-}

instance (Ord a, [a] ~ b) =>
         At (Trie b) where
    at xs = contains xs . iso (bool Nothing (Just ())) isJust
    {-# INLINE at #-}

singleton :: Foldable f => f a -> Trie [a]
singleton =
    foldr
        (\x -> Trie False . Map.singleton x)
        (Trie True Map.empty)

fromList
    :: (Ord a, Foldable f, Foldable g)
    => f (g a) -> Trie [a]
fromList = foldl' (flip insert) mempty
