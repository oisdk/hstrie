{-# LANGUAGE GADTs #-}

module Data.Trie.List.Map where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Control.Lens    hiding (children)

data Trie a b where
    Trie :: Maybe b -> Map a (Trie [a] b) -> Trie [a] b

endsHere :: Lens' (Trie a b) (Maybe b)
endsHere f (Trie e m) = fmap (flip Trie m) (f e)
{-# INLINE endsHere #-}

children :: Lens (Trie [a1] b) (Trie [a2] b) (Map a1 (Trie [a1] b)) (Map a2 (Trie [a2] b))
children f (Trie e m) = fmap (Trie e) (f m)
{-# INLINE children #-}
