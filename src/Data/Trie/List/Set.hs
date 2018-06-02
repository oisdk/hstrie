{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Trie.List.Set where

import           Prelude               hiding (filter)

import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map

import           Data.Bool             (bool)
import           Data.Foldable         (Foldable (..))
import           Data.Functor.Classes  (Eq1 (..), Ord1 (..))
import           Data.Semigroup        (Semigroup (..), stimesIdempotent)
import           Data.Maybe

import           Control.Applicative   (Applicative (..), liftA2)
import           Data.List             (unfoldr)

import           Control.Lens          hiding (children)

import           GHC.Exts              (IsList (Item))
import qualified GHC.Exts              as OverloadedLists

import           Data.Coerce.Utilities
import           Data.Trie.Internal.Ap

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

instance Eq1 Trie where
    liftEq eq (Trie xe xm) (Trie ye ym) =
        xe == ye &&
        (not xe || eq [] []) &&
        Map.size xm == Map.size ym && liftEq f (Map.toList xm) (Map.toList ym)
      where
        f (x,xt) (y,yt) =
            liftEq (\xs ys -> eq (x : xs) (y : ys)) xt yt

instance Ord1 Trie where
    liftCompare cmp (Trie xe xm) (Trie ye ym) =
        compare ye xe <>
        bool (cmp [] []) EQ xe <> liftCompare f (Map.toList xm) (Map.toList ym)
      where
        f (x,xt) (y,yt) =
            liftCompare (\xs ys -> cmp (x : xs) (y : ys)) xt yt

endsHere :: Lens (Trie a) (Trie a) Bool Bool
endsHere f (Trie e m) = fmap (flip Trie m) (f e)
{-# INLINE endsHere #-}

children :: Lens (Trie [a]) (Trie [b]) (Map a (Trie [a])) (Map b (Trie [b]))
children f (Trie e m) = fmap (Trie e) (f m)
{-# INLINE children #-}

instance Foldable Trie where
    foldr f b (Trie e m)
      | e = f [] r
      | otherwise = r
      where
        r = Map.foldrWithKey (\x tr xs -> foldr (f . (:) x) xs tr) b m
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
        go !n (Trie True m)  = Map.foldl' go (n + 1) m
    minimum tr@(Trie _ _) = unfoldr f tr
      where
        f :: Trie [a] -> Maybe (a, Trie [a])
        f (Trie True _)  = Nothing
        f (Trie False m) = Just (Map.findMin m)
    maximum tr@(Trie _ _) = unfoldr f tr
      where
        f :: Trie [a] -> Maybe (a, Trie [a])
        f (Trie False m) = Just (Map.findMax m)
        f (Trie True  m) = Map.lookupMax m

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
        (\x xs -> children . at x . nonEmpty %~ xs)
        (endsHere .~ True)

delete
    :: (Ord a, Foldable f)
    => f a -> Trie [a] -> Trie [a]
delete =
    foldr
        (\x xs -> children . at x . nonEmpty %~ xs)
        (endsHere .~ False)

nonEmpty :: Iso' (Maybe (Trie [a])) (Trie [a])
nonEmpty = anon (Trie False Map.empty) (\(Trie e m) -> not e && Map.null m)

member :: (Ord a, Foldable f) => f a -> Trie [a] -> Bool
member = foldr (\x -> anyOf (children . ix x)) (view endsHere)

type instance Index (Trie a) = a
type instance IxValue (Trie a) = ()

instance (Ord a, [a] ~ b) =>
         Contains (Trie b) where
    contains = foldr (\x xs -> (children . at x . nonEmpty . xs)) endsHere
    {-# INLINE contains #-}

instance (Ord a, [a] ~ b) =>
         Ixed (Trie b) where
    ix xs f tr
      | member xs tr = tr <$ f ()
      | otherwise = pure tr
    {-# INLINE ix #-}

instance (Ord a, [a] ~ b) =>
         At (Trie b) where
    at = foldr (\x xs -> children . at x . nonEmpty . xs) (endsHere . iso (bool Nothing (Just ())) isJust)
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

prefixed :: (Ord a, Foldable f) => f a -> Lens' (Trie [a]) (Trie [a])
prefixed = flip $ foldr (\x -> children . at x . nonEmpty)

-- filter :: ([a] -> Bool) -> Trie [a] -> Trie [a]
-- filter p (Trie e m) = Trie (e && p []) (Map.mapMaybeWithKey f m)
--   where
--     f k v = nonEmpty (filter (p . (:) k) v)
