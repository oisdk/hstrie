module Data.Coerce.Utilities
  (module Data.Coerce
  ,upon
  ,(#.)
  ,(.#))
  where

import Data.Coerce
import Data.Profunctor.Unsafe

infixr 1 `upon`
upon :: Coercible a b => (b -> b -> b) -> (a -> b) -> a -> a -> a
upon f _ = coerce f
{-# INLINE upon #-}
