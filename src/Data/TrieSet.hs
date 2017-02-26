{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}

module Data.TrieSet where

import           Control.Applicative
import           Control.Arrow       (first)
import           Control.Monad
import           Data.Foldable
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Semigroup

data Trie a where
        Trie :: Bool -> Map a (Trie [a]) -> Trie [a]

deriving instance Eq a => Eq (Trie [a])
deriving instance Ord a => Ord (Trie [a])

children :: Trie [a] -> Map a (Trie [a])
children (Trie _ c) = c

endHere :: Trie [a] -> Bool
endHere (Trie e _) = e

-- | Very lazy
-- >>> member "abc" (fromList ["abc", "def", ('c' : 'd' : undefined)])
-- True
-- >>> member "" (fromList ("" : "abc" : undefined))
-- True
-- >>> member [1] (fromList [[1], [1..]])
-- True
-- >>> member [1,2] (fromList [[1..]])
-- False
fromList
    :: (Ord a, Foldable f)
    => f [a] -> Trie [a]
fromList =
    uncurry (flip Trie . fmap fromList . Map.fromListWith (++)) .
    foldr f ([], False)
  where
    f []     = (, True) . fst
    f (x:xs) = first ((x, [xs]) :)

instance Ord a =>
         Semigroup (Trie [a]) where
    Trie v c <> Trie t d = Trie (v || t) (Map.unionWith mappend c d)

instance Ord a =>
         Monoid (Trie [a]) where
    mempty = Trie False mempty
    mappend = (<>)

insert
    :: (Foldable f, Ord a)
    => f a -> Trie [a] -> Trie [a]
insert = foldr f b
  where
    b :: Trie [b] -> Trie [b]
    b (Trie _ c) = Trie True c
    f
        :: Ord a
        => a -> (Trie [a] -> Trie [a]) -> Trie [a] -> Trie [a]
    f e a (Trie n c) = Trie n (Map.alter (Just . a . fold) e c)

instance Foldable Trie where
    foldr f b (Trie e c) =
        if e
            then f [] r
            else r
      where
        r = Map.foldrWithKey (flip . g . (:)) b c
        g k = foldr (f . k)

instance Show a =>
         Show (Trie [a]) where
  showsPrec d a =
    showParen (d >= 11)
      $ showString "fromList "
      . showsPrec 11 (toList a)

instance (Read a, Ord a) =>
         Read (Trie [a]) where
    readsPrec p =
        readParen (p > 10) $
        \r -> do
            ("fromList",s) <- lex r
            (xs,t) <- reads s
            return (fromList' xs, t)
      where
        fromList'
            :: Ord a
            => [[a]] -> Trie [a]
        fromList' = fromList

member
    :: (Foldable f, Ord a)
    => f a -> Trie [a] -> Bool
member = foldr f endHere
  where
    f e a = maybe False a . Map.lookup e . children

delete :: (Ord a, Foldable f)
       => f a -> Trie [a] -> Trie [a]
delete =
    foldr
        f
        (\(Trie _ m) ->
              Trie False m)
  where
    f
        :: Ord a
        => a -> (Trie [a] -> Trie [a]) -> Trie [a] -> Trie [a]
    f e a (Trie n c) = Trie n (Map.alter ((nilIfEmpty . a) =<<) e c)
    nilIfEmpty (Trie False m)
      | Map.null m = Nothing
    nilIfEmpty t = Just t

suffixes :: (Foldable f, Ord a) => f a -> Trie [a] -> Trie [a]
suffixes = foldr f id
  where
    f e a = foldMap a . Map.lookup e . children

prefixedBy :: (Foldable f, Ord a)
           => f a -> Trie [a] -> Trie [a]
prefixedBy xs = fold . foldr f Just xs
  where
    f e a = fmap (Trie False . Map.singleton e) . a <=< Map.lookup e . children

-- | Match longest string. Useful for parsers.
match
    :: (Monad f, Alternative f, Ord a)
    => ([[a]] -> f [a] -> f [a]) -> Trie [a] -> f a -> f [a]
match label t action = go t
  where
    go tr@(Trie e c) =
        label
            (toList tr)
            ((if e
                  then (<|> pure [])
                  else id)
                 (action >>= r c))
    r m c = maybe empty (fmap (c :) . go) (Map.lookup c m)
