module Trie 
  ( Trie
  
  -- * Query
  , null
  , size
  , member
  , complete
  , hasPref
  , hasSuff
  , hasSub
  , follow
  
  -- * Construction
  , empty
  , singleton
  
  -- * Insertion
  , insert
  
  -- * Delete/Update
  , delete
  , toggle
  , alter
  
  -- * Combine
  , difference
  , symmetricDifference
  , union
  , unions
  , intersection
  
  -- * Folds
  , foldrTrie
  
  -- * Lists
  , toList
  , fromList
  
  -- * Filters
  , begins
  , ends
  , subs
  
  -- * Debugging
  , showTrie
  ) where
    
import qualified Data.Map.Lazy as M
import Prelude hiding (null, foldr, any, all)
import Data.Foldable (Foldable, any, all, foldr, fold)
import Control.Applicative hiding (empty)
import Data.Functor (($>))
import Control.Monad
import Data.Monoid

{--------------------------------------------------------------------
  Trie type
--------------------------------------------------------------------}

data Trie a = Trie { endHere :: Bool 
                   , getTrie :: M.Map a (Trie a)
                   } deriving (Eq)                   

instance Show a => Show (Trie a) where
  show = show . toList
  
instance Ord a => Monoid (Trie a) where
  mempty  = empty
  mappend = union
  mconcat = unions

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is this the empty Trie?
null :: Trie a -> Bool
null (Trie e m) = not e && M.null m

-- | /O(n)/. The number of members in the Trie.
size :: Trie a -> Int
size (Trie e m) = M.foldr ((+) . size) (if e then 1 else 0) m

-- | /O(n)/. Is the element in the Trie?
member :: (Ord a, Foldable f) => f a -> Trie a -> Bool
member = follow False endHere

-- | /O(n)/. Does the Trie contain a member with this prefix?
--
-- > let t = fromList ["hello", "house", "roundabout"]
-- > hasPref "hell"  t == True
-- > hasPref "hello" t == True
-- > hasPref "abc"   t == False
hasPref :: (Ord a, Foldable f) => f a -> Trie a -> Bool
hasPref = follow False (not . null)

-- | /O(n * m)/, where m is the longest member of the Trie. 
-- Does the Trie contain a member with this suffix?
--
-- > let t = fromList ["hello", "house", "roundabout"]
-- > hasSuff "hell"  t == False
-- > hasSuff "about" t == True
hasSuff :: (Ord a, Foldable f) => f a -> Trie a -> Bool
hasSuff xs t = member xs t || any (hasSuff xs) (getTrie t)

-- | /O(n * m)/, where m is the longest member of the Trie. 
-- Does the Trie contain a member with this infix?
hasSub :: (Ord a, Foldable f) => f a -> Trie a -> Bool
hasSub xs t = hasPref xs t || any (hasSub xs) (getTrie t)

-- | /O(n)/. Creates a Trie of all of the completions of a given 
-- member.
--
-- > let t = fromList ["hello", "house", "roundabout"]
-- > complete "h" t == fromList ["ello", "ouse"]
complete :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
complete = follow empty id

-- | A universal function for querying a Trie with a 'Foldable'.
follow :: (Ord a, Foldable f) 
       => b -> (Trie a -> b) 
       -> f a -> Trie a -> b
follow base = foldr f where
  f e a = (maybe base a) . (M.lookup e . getTrie)
  
{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
  
-- | /O(1)/. The empty Trie.
empty :: Trie a
empty = Trie False M.empty

-- | /O(n)/. Constructs a Trie with one member.
singleton :: (Ord a, Foldable f) => f a -> Trie a
singleton = foldr (Trie False .: M.singleton) (Trie True M.empty)

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(n)/. Insert a new member into the Trie.
insert :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
insert = alter (Just empty) (\t -> t { endHere = True } )

{--------------------------------------------------------------------
  Delete/Update
--------------------------------------------------------------------}

-- | /O(n)/. Removes a member from the Trie.
delete :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
delete = alter (Nothing) (\t -> t { endHere = False } )

-- | /O(n)/. Removes a member from the Trie if it is present, or
-- inserts it if it is not.
toggle :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
toggle = alter (Just empty) (overEnd not)

-- | A universal function for modifying a Trie with a 'Foldable'.
alter :: (Ord a, Foldable f) 
      => (Maybe (Trie a)) 
      -> (Trie a -> Trie a) 
      -> f a -> Trie a  -> Trie a
alter o i = fold .: foldr f (nilIfEmpty . i) where
  f e a = nilIfEmpty . overMap (M.alter (a <=< (<|> o)) e)
  
{--------------------------------------------------------------------
  Combine
--------------------------------------------------------------------}
-- | Returns a Trie of the members of the first Trie not existing in
-- the second Trie.
difference :: Ord a => Trie a -> Trie a -> Trie a
difference = foldrTrie delete

-- | Returns a Trie of the members that exist in eithe the first 
-- Trie, or the second Trie, or both.
symmetricDifference :: Ord a => Trie a -> Trie a -> Trie a
symmetricDifference = foldrTrie toggle

mergeBy :: (M.Map a (Trie a) 
        -> M.Map b (Trie b) 
        -> M.Map c (Trie c)) 
        -> (Bool -> Bool -> Bool) 
        -> Trie a -> Trie b -> Trie c
mergeBy fm fb (Trie a m) (Trie b n) = Trie (fb a b) (fm m n)

-- | Returns a Trie of the members that exist in either Trie.
union :: Ord a => Trie a -> Trie a -> Trie a
union = mergeBy (M.unionWith union) (||)

-- | Union of a foldable of Tries.
unions :: (Ord a, Foldable f) => f (Trie a) -> Trie a
unions = foldr union empty

intersectionWith :: Ord d 
                 => (a -> b -> Maybe c) 
                 -> M.Map d a 
                 -> M.Map d b 
                 -> M.Map d c
intersectionWith f = M.mergeWithKey 
                     (const f) 
                     (const M.empty) 
                     (const M.empty)

-- | Returns a Trie of the elements in both the first and second Trie
intersection :: Ord a => Trie a  -> Trie a -> Trie a
intersection = mergeBy 
               (intersectionWith $ nilIfEmpty .: intersection) 
               (&&)

symmetricDifferenceWith :: Ord d 
                        => (a -> a -> Maybe a)
                        -> M.Map d a
                        -> M.Map d a
                        -> M.Map d a
symmetricDifferenceWith f = M.mergeWithKey (const f) id id

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | Folds over the members of a Trie, in list form.
foldrTrie :: ([a] -> b -> b) -> b -> Trie a -> b
foldrTrie f i (Trie a m) = M.foldrWithKey ff s m where
  s    = if a then f [] i else i
  ff k = flip (foldrTrie $ f . (k :))
  
{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | Converts a Trie to a list of its members, in list form.
toList :: Trie a -> [[a]]
toList = foldrTrie (:) []

-- | Creates a Trie from a 'Foldable' of 'Foldable's.
fromList :: (Ord a, Foldable f, Foldable g) => g (f a) -> Trie a
fromList = foldr insert empty

{--------------------------------------------------------------------
  Filters
--------------------------------------------------------------------}
  
-- | Returns a Trie of the members that begin with the given prefix.
begins :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
begins = fold .: foldr f Just where
  f e a = fmap (Trie False . M.singleton e) . a <=< M.lookup e . getTrie
                        
-- | Returns a Trie of the members that end with the given suffix.
ends :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
ends = infs ((Trie True M.empty <$) . guard . endHere)

-- | Returns a Trie of the members that contain the given infix.    
subs :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
subs = infs Just

infs :: (Ord a, Foldable f) 
     => (Trie a -> Maybe (Trie a)) 
     -> f a -> Trie a -> Trie a
infs i xs = fold . infs' xs where
  infs' = foldr f i
  f e a (Trie _ m) = Trie False <$> tryComb (a =<< M.lookup e m) where 
    tryComb Nothing  | M.null lowMap = Nothing
                     | otherwise     = Just lowMap
    tryComb (Just c) | M.null lowMap = Just (M.singleton e c)
                     | otherwise     = Just (M.insertWith union e c lowMap)
    lowMap = M.mapMaybe (infs' xs) m
    
{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}

-- | A textual representation of the Trie, suitable for debugging.
--
-- > let t = fromList $ words "house car roundabout round rounders \
-- >                          \carpet cat cap carpentry"
-- > putStrLn (showTrie t)
-- >
-- > 'c' 'a' 'p'|
-- >         'r'|'p' 'e' 'n' 't' 'r' 'y'|
-- >                     't'|
-- >         't'|
-- > 'h' 'o' 'u' 's' 'e'|
-- > 'r' 'o' 'u' 'n' 'd'|'a' 'b' 'o' 'u' 't'|
-- >                     'e' 'r' 's'|
showTrie :: Show a => Trie a -> String
showTrie = unlines . showTrie' where 
  showTrie' = (ff <=< M.assocs) . getTrie where
    ff (k,t) = zipWith (++) pads $ case showTrie' t of [] -> [[]]
                                                       r  -> r 
                                                       where
      pads = (strK ++ if endHere t then "|" else " ") : repeat padK
      strK = show k
      padK = ' ' : zipWith const (repeat ' ') strK

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}

-- | Takes a predicate and a value, and returns Just the value if the
-- predicate on the value evaluates to True, otherwise returns
-- Nothing.
ensure :: (a -> Bool) -> a -> Maybe a
ensure f x = guard (f x) $> x

nilIfEmpty :: Trie a -> Maybe (Trie a)
nilIfEmpty = ensure (not . null)

overEnd :: (Bool -> Bool) -> Trie a -> Trie a
overEnd f (Trie e m) = Trie (f e) m

overMap :: Ord b 
        => (M.Map a (Trie a) 
        -> M.Map b (Trie b)) 
        -> Trie a -> Trie b
overMap f (Trie e m) = Trie e (f m)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)
