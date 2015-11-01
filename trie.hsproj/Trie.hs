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
  
  -- * Debugging
  , showTrie
  ) where
    
import qualified Data.Map.Lazy as M
import Prelude hiding (null, foldr, any, all)
import Data.Maybe (fromMaybe)
import Data.Foldable (Foldable, any, all, foldr)
import Control.Applicative ((<|>), (<$>))
import Data.Monoid

{--------------------------------------------------------------------
  Trie type
--------------------------------------------------------------------}

data Trie a = Trie { getTrie :: M.Map a (Trie a)
                   , endHere :: Bool } deriving (Eq)                   

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
null (Trie m e) = not e && M.null m

-- | /O(n)/. The number of members in the Trie.
size :: Trie a -> Int
size (Trie m e) = M.foldr ((+) . size) (if e then 1 else 0) m

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

-- | A universal function for querying a Trie with a 'Foldable'. When
-- calling @'follow' base f@, a function which takes a 
-- 'Foldable' and a Trie is created such that the 'Foldable' is 
-- followed until either the 'Foldable' runs out, or its next element
-- can't be found in the Trie.
--
-- * If the 'Foldable' is exhausted, 'f' is called on the remaining
-- Trie.
--
-- * If the next element in the 'Foldable' cannot be found, 'base' is
-- returned.
--
-- This function can be thought of as parallel to 'alter', but it 
-- discards the Trie that's being queried, and only keeps the final 
-- level reached with the 'Foldable'.
--
-- Examples:
--
-- * Member
--
-- > member = follow False endHere
-- 
-- Here, the first argument 'False' is returned if any element of the 
-- 'Foldable' cannot be found, and 'endHere' is called on the final 
-- Trie found. i.e., this function follows the 'Foldable' along the 
-- Trie, until it can't find the next element, so it returns 'False',
-- or it ends, so it asks the current Trie if it's a final Trie.
--
-- * Complete
--
-- > complete = follow empty id
--
-- Here, the empty Trie is returned if the entire 'Foldable' can't be 
-- found, or the final Trie you reach is returned if the 'Foldable' 
-- is exhausted.
follow :: (Ord a, Foldable f) 
       => b -> (Trie a -> b) 
       -> f a -> Trie a -> b
follow base = foldr f where
  f e a = fromMaybe base . fmap a . M.lookup e . getTrie 
  
{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
  
-- | /O(1)/. The empty Trie.
empty :: Trie a
empty = Trie M.empty False

-- | /O(n)/. Constructs a Trie with one member.
singleton :: (Ord a, Foldable f) => f a -> Trie a
singleton = foldr 
            ((flip Trie False .) . M.singleton) 
            (Trie M.empty True)

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(n)/. Insert a new member into the Trie.
insert :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
insert = alter (. fromMaybe empty) (overEnd (const True))

{--------------------------------------------------------------------
  Delete/Update
--------------------------------------------------------------------}

-- | /O(n)/. Removes a member from the Trie.
delete :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
delete = alter (=<<) (overEnd (const False))

-- | /O(n)/. Removes a member from the Trie if it is present, or
-- inserts it if it is not.
toggle :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
toggle = alter (. fromMaybe empty) (overEnd not)


-- | A universal function for modifying a Trie with a 'Foldable'. 
-- When called with two arguments, @'alter' atFind atEnd@, a function
-- is created, such that
--
-- * atEnd is called on the lowest level Trie, i.e., the one reached 
-- if the entire 'Foldable' is followed and exhausted.
--
-- * atFind is called on the looked-up Trie at every element of the
-- 'Foldable'. This should apply the /rest/ of the alter function,
-- with the /rest/ of the 'Foldable', depending on the result of the
-- lookup. 
--
-- This can be thought of as parallel to 'follow', where 'follow' 
-- follows a 'Foldable', and then deals with either the Foldable 
-- being exhausted, or the next element not being found. 'alter' 
-- similarly follows a 'Foldable' down a Trie, but it preserves the 
-- rest of the Trie, and performs the required cleanup. (i.e, a 
-- branch of a Trie with no 'endHere's set to 'True' is not 
-- preserved.)
--
-- Examples:
--
-- * Insert
--
-- > insert = alter (. fromMaybe empty) (overEnd (const True))
--
-- Here, the first argument to alter says that if the next element of 
-- the 'Foldable' can't be found, then just give an empty Trie in its 
-- place. When the 'Foldable' is exhausted, though, call 
-- 'overEnd (const True)' on the final Trie found. (
-- 'overEnd (const True)' sets the boolean end flag to True.)
--
-- * Delete
--
-- > delete = alter (=<<) (overEnd (const False))
--
-- Here, the first argument says that if the next element cannot be 
-- found, pass the 'Nothing' back up along the chain. Otherwise, 
-- turns the end to 'False'.
alter :: (Ord a, Foldable f) 
      => (( Trie a 
         -> Maybe (Trie a)) 
         -> Maybe (Trie a) 
         -> Maybe (Trie a)) 
      -> (Trie a -> Trie a) 
      -> f a -> Trie a  -> Trie a
alter o i = (fromMaybe empty .) . foldr f (nilIfEmpty . i) where
  f e a = nilIfEmpty . overMap (M.alter (o a) e)
  
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
mergeBy fm fb (Trie m a) (Trie n b) = Trie (fm m n) (fb a b)

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
               (intersectionWith $ (nilIfEmpty .) . intersection) 
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
foldrTrie f i (Trie m a) = M.foldrWithKey ff s m where
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
begins xs = fromMaybe empty . begins' xs . Just where
  begins' = foldr f id
  f e a   = (fmap (flip Trie False . M.singleton e) 
            . a 
            . M.lookup e 
            . getTrie =<<)
            
-- | Returns a Trie of the members that end with the given suffix.
ends :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
ends xs = fromMaybe empty . ends' xs where
  ends' = foldr f someIfEnd
  someIfEnd t | endHere t = Just (Trie M.empty True)
              | otherwise = Nothing
  f e a (Trie m _) = union rest <$> head <|> nilIfEmpty rest where
    head = flip Trie False <$> M.singleton e <$> (a =<< M.lookup e m)
    rest = Trie (M.mapMaybe (ends' xs) m) False

  
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
showTrie = showTrie' "" where 
  showTrie' b (Trie m e) = tryAdd e b 
                         . fmap (minit . f) 
                         $ (M.assocs m) where
    f (h,t) = str ++ showTrie' (pad ++ b) t where
      str = show h
      pad = ' ' : zipWith const (repeat ' ') str
  minit []        = []
  minit ('\n':[]) = []
  minit (x:xs)    = x : minit xs
  tryAdd False b = unlines . zipWith (++) (" " : repeat (' ':b))
  tryAdd True  b = ('|' :) 
                 . unlines 
                 . zipWith (++) ("" : repeat (' ':b))

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}

nilIfEmpty :: Trie a -> Maybe (Trie a)
nilIfEmpty = ifMaybe (not . null)

overEnd :: (Bool -> Bool) -> Trie a -> Trie a
overEnd f (Trie m e) = Trie m (f e)

ifMaybe :: (a -> Bool) -> a -> Maybe a
ifMaybe f x | f x       = Just x
            | otherwise = Nothing

overMap :: Ord b 
        => (M.Map a (Trie a) 
        -> M.Map b (Trie b)) 
        -> Trie a -> Trie b
overMap f (Trie m e) = Trie (f m) e