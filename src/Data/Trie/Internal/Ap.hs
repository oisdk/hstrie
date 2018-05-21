{-# options_ghc -fno-warn-noncanonical-monoid-instances #-}

module Data.Trie.Internal.Ap where

import           Control.Applicative
import           Data.Coerce.Utilities
import           Data.Semigroup

newtype Ap f a = Ap
    { getAp :: f a
    }

instance (Applicative f, Semigroup a) =>
         Semigroup (Ap f a) where
    (<>) = liftA2 (<>) `upon` getAp
    {-# INLINE (<>) #-}

instance (Applicative f, Monoid a) =>
         Monoid (Ap f a) where
    mappend = liftA2 mappend `upon` getAp
    {-# INLINE mappend #-}
    mempty = Ap (pure mempty)
    {-# INLINE mempty #-}
