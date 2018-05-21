{-# LANGUAGE UndecidableInstances #-}

module Data.Trie.Vector.Set where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector

import           Data.Semigroup
import           Data.Foldable

import           Data.These
import           Data.Align

import           GHC.Base (augment)

data Trie a where
        Trie :: Vector a -> Bool -> Map a (Trie [a]) -> Trie [a]

deriving instance (Eq a, b ~ [a]) => Eq (Trie b)
deriving instance (Ord a, b ~ [a]) => Ord (Trie b)

instance (Show a, b ~ [a]) => Show (Trie b) where
    showsPrec n = showsPrec n . toList

instance (Ord a, b ~ [a]) =>
         Semigroup (Trie b) where
    Trie xs xe xm <> Trie ys ye ym =
        Vector.ifoldr
            f
            (Trie xs (xe || ye) (Map.unionWith (<>) xm ym))
            (align xs ys)
      where
        f i (These x y) a
          | x == y = a
          | otherwise =
              Trie
                  (Vector.take i xs)
                  False
                  (Map.fromList
                       [ (x, Trie (Vector.drop (i + 1) xs) xe xm)
                       , (y, Trie (Vector.drop (i + 1) ys) ye ym)])
        f i (This x) _ =
            Trie ys ye (Map.insertWith (<>) x (Trie (Vector.drop (i + 1) xs) xe xm) ym)
        f i (That y) _ =
            Trie xs xe (Map.insertWith (<>) y (Trie (Vector.drop (i + 1) ys) ye ym) xm)

instance (Ord a, b ~ [a]) =>
         Monoid (Trie b) where
    mappend = (<>)
    mempty = Trie Vector.empty False Map.empty

instance Foldable Trie where
    foldMap f (Trie v e m)
      | e = f (Vector.toList v) `mappend` r
      | otherwise = r
      where
        r =
            Map.foldMapWithKey
                (\x -> foldMap (f . augment (\c n -> Vector.foldr c (c x n) v)))
                m

singleton :: Vector a -> Trie [a]
singleton xs = Trie xs True Map.empty

-- |
-- >>> foldr (insert . Vector.fromList) mempty ["ab", "abc", "def"]
-- ["ab","abc","def"]
insert :: Ord a => Vector a -> Trie [a] -> Trie [a]
insert = (<>) . singleton

member :: (Ord a, Foldable f) => f a -> Trie [a] -> Bool
member = foldr f b
  where
    b :: Trie a -> Bool
    b (Trie xs e _) = Vector.null xs && e
    f
        :: Ord a
        => a -> (Trie [a] -> Bool) -> Trie [a] -> Bool
    f x xs (Trie ys e m) =
        case uncons ys of
            Nothing -> any xs (Map.lookup x m)
            Just (z,zs) -> x == z && xs (Trie zs e m)

uncons :: Vector a -> Maybe (a, Vector a)
uncons xs
  | Vector.null xs = Nothing
  | otherwise = Just (Vector.unsafeHead xs, Vector.unsafeTail xs)

delete :: (Ord a) => Vector a -> Trie [a] -> Trie [a]
delete xs (Trie ys e m) = Vector.ifoldr f b (align xs ys)
  where
    b = Trie ys False m
    f i (This x) _ =
        Trie ys e (Map.alter (nonEmpty . delete (Vector.drop (i+1) xs) =<<) x m)
    f _ (That _) _ = Trie ys e m
    f _ (These x y) a
        | x == y = a
        | otherwise = Trie ys e m

nonEmpty :: Trie a -> Maybe (Trie a)
nonEmpty tr@(Trie xs e m)
  | not e =
      case Map.size m of
          0 -> Nothing
          1 ->
              case Map.elemAt 0 m of
                  (k,Trie ys ye ym) ->
                      Just (Trie (xs <> Vector.singleton k <> ys) ye ym)
          _ -> Just tr
  | otherwise = Just tr
