{-# LANGUAGE UndecidableInstances #-}

module Data.Trie.Vector.Set where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector

import           Control.Lens    hiding (children)

import           Data.Semigroup
import           Data.Foldable

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
    xt@(Trie xs xe xm) <> yt@(Trie ys ye ym) = go 0
      where
        go !i =
            case (xs Vector.!? i, ys Vector.!? i) of
                (Just x,Just y) ->
                    case compare x y of
                        EQ -> go (i+1)
                        LT -> Trie (Vector.take i xs) False (Map.fromDistinctAscList [(x, xd), (y, yd)])
                        GT -> Trie (Vector.take i xs) False (Map.fromDistinctAscList [(y, yd), (x, xd)])
                (Just x,Nothing) -> yt & children . at x <>~ Just xd
                (Nothing,Just y) -> xt & children . at y <>~ Just yd
                (Nothing,Nothing) ->
                    Trie xs (xe || ye) (Map.unionWith (<>) xm ym)
          where
            yd = yt & prefix %~ Vector.unsafeDrop (i + 1)
            xd = xt & prefix %~ Vector.unsafeDrop (i + 1)
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
member ks = foldr f b ks 0
  where
    b :: Int -> Trie [a] -> Bool
    b !i (Trie xs e _) = Vector.length xs == i && e
    f :: a -> (Int -> Trie [a] -> Bool) -> Int -> Trie [a] -> Bool
    f x xs i tr@(Trie ys _ m) =
        case ys Vector.!? i of
            Nothing -> any (xs 0) (Map.lookup x m)
            Just y -> x == y && xs (i+1) tr

delete :: forall a f. (Ord a, Foldable f) => f a -> Trie [a] -> Trie [a]
delete xs = foldr f b xs 0
  where
    b :: Int -> Trie [a] -> Trie [a]
    b !i tr@(Trie ys _ m)
        | i == Vector.length ys = Trie ys False m
        | otherwise = tr
    f :: a -> (Int -> Trie [a] -> Trie [a]) -> Int -> Trie [a] -> Trie [a]
    f x a !i tr@(Trie ys _ _) = case ys Vector.!? i of
      Nothing -> tr & (children . at x) %~ (nonEmpty . a 0 =<<)
      Just y | x == y -> a (i+1) tr
             | otherwise -> tr

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

instance Plated (Trie a) where
    plate f (Trie xs e m) = fmap (Trie xs e) (traverse f m)
    {-# INLINE plate #-}
