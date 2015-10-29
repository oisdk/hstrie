import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Foldable hiding (toList)
import Prelude hiding (foldr, null, all, any)
import Control.Applicative hiding (empty)
import qualified Data.List as L
import Control.Monad

data Trie a = Trie { getTrie :: M.Map a (Trie a), endHere :: Bool } deriving (Eq)

instance Show a => Show (Trie a) where
  show  = show . toList

-- | overMap applies its first argument to the Map in the Trie of its second
-- argument.
overMap :: Ord b => (M.Map a (Trie a) -> M.Map b (Trie b)) -> Trie a -> Trie b
overMap f (Trie m e) = Trie (f m) e


-- | zipUntil constructs a function that acts over a foldable and a Trie.
-- The function constructed will follow the Trie and the foldable together,
-- until either the next element can't be found in the Trie, so it evaluates
-- to its first argument, or the foldable is exhausted, so it evaluates its
-- second argument on the nested Trie it finished on.
zipUntil :: (Ord a, Foldable f) => b -> (Trie a -> b) -> f a -> Trie a -> b
zipUntil base = foldr f where
  f e a = fromMaybe base . fmap a . M.lookup e . getTrie 
  
-- | Is True iff the foldable is contained in the Trie
contains :: (Ord a, Foldable f) => f a -> Trie a -> Bool
contains = zipUntil False endHere

containsL :: Eq a => [a] -> [[a]] -> Bool
containsL = any . (==)

-- | Evaluates to a Trie of the completions of the foldable
complete :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
complete = zipUntil empty id

-- | Checks that a function on a trie and a function on a list 
-- of lists is equivalent
check :: (Ord a, Eq c) => ([a] -> Trie a -> c) -> ([a] -> [[a]] -> c) -> [[a]] -> Bool
check tf lf l = all (liftM2 (==) tfa lfa) (liftM2 (++) L.tails L.inits =<< l)  where
  t   = fromList l
  tfa = flip tf t
  lfa = flip lf l
  
-- | Evaluates to a Trie of all of the members which begin with
-- the foldable
begins :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
begins = foldr f id where
      f e a = toTrie . fmap (flip Trie False . M.singleton e . a) . M.lookup e . getTrie
      
beginsL :: Ord a => [a] -> [[a]] -> Trie a
beginsL l = fromList . filter (L.isPrefixOf l)
      
insert :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
insert = foldr f toTrue where
  f e a = overMap (M.alter (Just . a . toTrie) e)

null :: Trie a -> Bool
null (Trie m e) = M.null m

empty :: Trie a
empty = Trie M.empty False

toTrue :: Trie a -> Trie a
toTrue (Trie m _) = Trie m True

toTrie :: Maybe (Trie a) -> Trie a
toTrie = fromMaybe empty

toFalse :: Trie a -> Trie a
toFalse (Trie m _) = Trie m False

nilIfEmpty :: Trie a -> Maybe (Trie a)
nilIfEmpty t@(Trie m e) | M.null m && not e = Nothing
                        | otherwise         = Just t
             
someIfEmpty :: Trie a -> Maybe (Trie a)
someIfEmpty t | null t = Just t
              | otherwise = Nothing

             
getSubs :: (Ord a, Foldable f) => (Trie a -> Maybe (Trie a)) -> 
                                  (Trie a -> Maybe (Trie a)) -> 
                                   f a -> Trie a -> Trie a
getSubs b g xs = toTrie . getSubs' xs where
  getSubs' = foldr f b
  f e a = g . overMap (M.mapMaybeWithKey ff) where
    ff k v | k == e    = a v <|> getSubs' xs v
           | otherwise = getSubs' xs v
           
minit :: String -> String
minit [] = []
minit ('\n':[]) = []
minit (x:xs) = x : minit xs

pad :: Int -> String -> String
pad n s = (take n) (s ++ (repeat ' '))

debugPrint :: Show a => Trie a -> String
debugPrint = debugPrint' "" where 
    debugPrint' b (Trie m e) = unlines $ zipWith (++) 
                                      ((if e then "|" else " ") : repeat b) 
                                      (minit . f <$> M.assocs m) where
                                       f = \(h,t) -> (pad 0 (show h)) ++ (debugPrint' ("   " ++ b) t)
           
--wiSubs :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
--wiSubs = getSubs nilIfEmptyEnd (nilIfEmptyEnd . toFalse)
--
--ends :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
--ends = getSubs someIfEmpty (nilIfEmptyEnd . toFalse)
           


fromList :: (Ord a, Foldable f, Foldable g) => f (g a) -> Trie a
fromList = foldr insert empty

toList :: Trie a -> [[a]]
toList (Trie m a) | a         = [] : rest
                  | otherwise = rest where
                  rest = M.assocs m >>= uncurry (fmap . (:)) . fmap toList
                  
count :: Trie a -> Int
count (Trie m e) = M.foldr ((+) . count) (if e then 1 else 0) m

instance Ord a => Ord (Trie a) where
  compare (Trie a _) (Trie b _) = case compare x y of LT -> LT
                                                      EQ -> compare w z
                                                      GT -> GT
                                                      where (x,w) = M.findMax a
                                                            (y,z) = M.findMax b
                                                                                                                        