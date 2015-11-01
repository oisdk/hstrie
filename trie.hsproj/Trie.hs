module Trie 
  ( Trie
  
  -- * Query
  , null
  , size
  , member
  ) where
    
import qualified Data.Map.Lazy as M
import Prelude hiding (null, foldr)
import Data.Maybe (fromMaybe)
import Data.Foldable (Foldable, foldr)


data Trie a = Trie { getTrie :: M.Map a (Trie a)
                   , endHere :: Bool } deriving (Eq)
                   

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is this the empty Trie?
null :: Trie a -> Bool
null (Trie m e) = not e && M.null m

-- | /O(n)/. The number of members in the Trie.
size :: Trie a -> Int
size (Trie m e) = M.foldr ((+) . size) (if e then 1 else 0) m

-- | /O(n)/. Is the element in the Trie?
member :: (Ord a, Foldable f) => f a -> Trie a -> Bool
member = followUntil False endHere

-- | /O(n)/. Does the Trie contain a member with this prefix?
--
-- > let t = fromList ["hello", "house", "roundabout"]
-- > hasPref "hell"  t == True
-- > hasPref "hello" t == True
-- > hasPref "abc"   t == False
hasPref :: (Ord a, Foldable f) => f a -> Trie a -> Bool
hasPref = followUntil False (const True)

-- | A universal function for querying a Trie with a 'Foldable'. When
-- calling @'followUntil' base f@, a function which takes a 
-- 'Foldable' and a Trie is created such that the 'Foldable' is 
-- followed until either the 'Foldable' runs out, or its next element
-- can't be found in the Trie.
--
-- * If the 'Foldable' is exhausted, 'f' is called on the remaining
-- Trie.
--
-- * If the next element in the 'Foldable' cannot be found, 'base' is
-- returned.
followUntil :: (Ord a, Foldable f) => b -> (Trie a -> b) -> f a -> Trie a -> b
followUntil base = foldr f where
  f e a = fromMaybe base . fmap a . M.lookup e . getTrie 