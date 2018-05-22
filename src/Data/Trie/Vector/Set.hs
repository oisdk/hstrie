{-# LANGUAGE UndecidableInstances #-}

module Data.Trie.Vector.Set where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector

import           Control.Lens    hiding (children,uncons)

import           Data.Semigroup
import           Data.Foldable

import           Data.These
import           Data.Align

import           GHC.Base (augment)

import           Control.DeepSeq (NFData(rnf))

data Trie a where
        Trie :: {-# UNPACK #-} !(Vector a) -> !Bool -> !(Map a (Trie [a])) -> Trie [a]

deriving instance (Eq a, b ~ [a]) => Eq (Trie b)
deriving instance (Ord a, b ~ [a]) => Ord (Trie b)

instance (NFData a, b ~ [a]) => NFData (Trie b) where
    rnf (Trie xs _ m) = rnf xs `seq` rnf m

instance (Show a, b ~ [a]) => Show (Trie b) where
    showsPrec n = showsPrec n . toList

prefix :: Lens' (Trie [a]) (Vector a)
prefix f (Trie xs e m) = fmap (\ys -> Trie ys e m) (f xs)
{-# INLINE prefix #-}

endsHere :: Lens' (Trie a) Bool
endsHere f (Trie xs e m) = fmap (\e' -> Trie xs e' m) (f e)
{-# INLINE endsHere #-}

children :: Lens' (Trie [a]) (Map a (Trie [a]))
children f (Trie xs e m) = fmap (Trie xs e) (f m)
{-# INLINE children #-}

instance (Ord a, b ~ [a]) =>
         Semigroup (Trie b) where
    (Trie xs xe xm) <> (Trie ys ye ym) = go 0
      where
        go !i =
            case (xs Vector.!? i, ys Vector.!? i) of
                (Just x,Just y) ->
                    case compare x y of
                        EQ -> go (i + 1)
                        LT ->
                            Trie (Vector.take i xs) False (Map.fromDistinctAscList
                                     [ (x, Trie (Vector.drop (i + 1) xs) xe xm)
                                     , (y, Trie (Vector.drop (i + 1) ys) ye ym)])
                        GT ->
                            Trie (Vector.take i xs) False (Map.fromDistinctAscList
                                     [ (y, Trie (Vector.drop (i + 1) ys) ye ym)
                                     , (x, Trie (Vector.drop (i + 1) xs) xe xm)])
                (Just x,Nothing) ->
                    Trie ys ye (Map.insertWith (<>) x
                             (Trie (Vector.drop (i + 1) xs) xe xm)
                             ym)
                (Nothing,Just y) ->
                    Trie xs xe (Map.insertWith (<>) y
                             (Trie (Vector.drop (i + 1) ys) ye ym)
                             xm)
                (Nothing,Nothing) ->
                    Trie xs (xe || ye) (Map.unionWith (<>) xm ym)
    {-# INLINE (<>) #-}

instance (Ord a, b ~ [a]) =>
         Monoid (Trie b) where
    mappend = (<>)
    {-# INLINE mappend #-}
    mempty = Trie Vector.empty False Map.empty
    {-# INLINE mempty #-}

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
{-# INLINE singleton #-}

-- |
-- >>> foldr (insert . Vector.fromList) mempty ["ab", "abc", "def"]
-- ["ab","abc","def"]
insert :: Ord a => Vector a -> Trie [a] -> Trie [a]
insert = (<>) . singleton
{-# INLINE insert #-}

member :: forall a f. (Ord a, Foldable f) => f a -> Trie [a] -> Bool
member = foldr f b
  where
    b :: Trie [a] -> Bool
    b (Trie xs e _) = Vector.null xs && e
    f :: a -> (Trie [a] -> Bool) -> Trie [a] -> Bool
    f x xs (Trie ys e m) =
        case uncons ys of
            Nothing -> any xs (Map.lookup x m)
            Just (z,zs) -> x == z && xs (Trie zs e m)

uncons :: Vector a -> Maybe (a, Vector a)
uncons xs
  | Vector.null xs = Nothing
  | otherwise = Just (Vector.unsafeHead xs, Vector.unsafeTail xs)
{-# INLINE uncons #-}

delete :: (Ord a) => Vector a -> Trie [a] -> Trie [a]
delete xs (Trie ys e m) = Vector.ifoldr f (Trie ys False m) (align xs ys)
  where
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
              case Map.findMin m of
                  (k,Trie ys ye ym) ->
                      Just (Trie (xs <> Vector.singleton k <> ys) ye ym)
          _ -> Just tr
  | otherwise = Just tr

fromList :: (Foldable f, Ord a) => f (Vector a) -> Trie [a]
fromList = foldl' (flip insert) mempty
{-# INLINE fromList #-}
