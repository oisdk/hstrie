module Trie (
  Trie
  , empty
  , null
  , insert
  , count
  , show
  , fromList
  , toList
  , contains
  , hasPref
  , hasSuff
  , complete
  , begins
  , remove
  , hasSub
  , debugPrint
  , toListAsc
  , toListDesc
  , foldrTrie
  , foldrTrieGen
  , xor
  , union
  , ends) where

import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe)
import Data.Foldable hiding (toList)
import Prelude hiding (foldr, null, all, any)
import Control.Applicative hiding (empty)
import Data.Monoid
import Control.Monad

data Trie a = Trie { getTrie :: M.Map a (Trie a)
                   , endHere :: Bool } deriving (Eq)

empty :: Trie a
empty = Trie M.empty False

ifMaybe :: (a -> Bool) -> a -> Maybe a
ifMaybe f x | f x       = Just x
            | otherwise = Nothing

null :: Trie a -> Bool
null (Trie m e) = not e && M.null m

noEnd :: Trie a -> Trie a
noEnd = overEnd (const False)

insert :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
insert = alter (Just . overEnd (const True)) (. fromMaybe empty)
  
remove :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
remove = alter (nilIfEmpty . noEnd) (=<<)
    
xor :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
xor = alter (nilIfEmpty . overEnd not) (. fromMaybe empty)

union :: Ord a => Trie a -> Trie a -> Trie a
union (Trie m a) (Trie n b) = Trie (M.unionWith union m n) (a || b)

unions :: (Ord a, Foldable f) => f (Trie a) -> Trie a
unions = foldr union empty

instance Show a => Show (Trie a) where
  show = show . toList
  
instance Ord a => Monoid (Trie a) where
  mempty  = empty
  mappend = union
  mconcat = unions

fromList :: (Ord a, Foldable f, Foldable g) => f (g a) -> Trie a
fromList = foldr insert empty

makeList :: ([[a]] -> [[a]] -> [[a]]) -> Trie a -> [[a]]
makeList com (Trie m a) = M.foldlWithKey f (if a then [[]] else []) m where
  f c k = (com c) . map (k :) . makeList com

toListAsc :: Trie a -> [[a]]
toListAsc = makeList (++)

toListDesc :: Trie a -> [[a]]
toListDesc = makeList (flip (++))

foldrTrie :: ([a] -> b -> b) -> b -> Trie a -> b
foldrTrie f i (Trie m a) | a         = M.foldrWithKey ff (f [] i) m 
                         | otherwise = M.foldrWithKey ff i m where
  ff k = flip (foldrTrie $ f . (k :))
    
toList :: Trie a -> [[a]]
toList = foldrTrie (:) []

filter :: Ord a => ([a] -> Bool) -> Trie a -> Trie a
filter f = foldrTrie ff empty where
  ff e a | f e       = insert e a
         | otherwise = a

foldrTrieGen :: (Applicative f, Foldable f, Monoid (f a)) => (f a -> b -> b) 
                                                          -> b -> Trie a -> b
foldrTrieGen f i (Trie m a) = M.foldrWithKey ff (if a then f mempty i else i) m where
  ff k = flip (foldrTrieGen $ f . mappend (pure k))

contains :: (Ord a, Foldable f) => f a -> Trie a -> Bool
contains = zipUntil False endHere

hasPref :: (Ord a, Foldable f) => f a -> Trie a -> Bool
hasPref = zipUntil False (const True)

hasSuff :: (Ord a, Foldable f) => f a -> Trie a -> Bool
hasSuff xs t = contains xs t || any (hasSuff xs) (getTrie t)

complete :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
complete = zipUntil empty id
  
begins :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
begins xs = fromMaybe empty . begins' xs . Just where
  begins' = foldr f id
  f e a   = ((flip Trie False . M.singleton e <$>) 
            . a . M.lookup e . getTrie =<<)
            
ends :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
ends xs = fromMaybe empty . ends' xs where
  ends' = foldr f someIfEnd
  someIfEnd t | endHere t = Just (Trie M.empty True)
              | otherwise = Nothing
  f e a (Trie m _) = union rest <$> head <|> nilIfEmpty rest where
    head = flip Trie False <$> M.singleton e <$> (a =<< M.lookup e m)
    rest = Trie (M.mapMaybe (ends' xs) m) False

  
hasSub :: (Ord a, Foldable f) => f a -> Trie a -> Bool
hasSub xs t = hasPref xs t || any (hasSub xs) (getTrie t)

overMap :: Ord b => (M.Map a (Trie a) -> M.Map b (Trie b)) -> Trie a -> Trie b
overMap f (Trie m e) = Trie (f m) e

nilIfEmpty :: Trie a -> Maybe (Trie a)
nilIfEmpty = ifMaybe (not . null)

overEnd :: (Bool -> Bool) -> (Trie a -> Trie a)
overEnd f (Trie m e) = Trie m (f e)

minit :: String -> String
minit []        = []
minit ('\n':[]) = []
minit (x:xs)    = x : minit xs

tryAdd :: Bool -> String -> [String] -> String
tryAdd False b = unlines . zipWith (++) (" " : repeat (' ':b))
tryAdd True  b = ('|' :) . unlines . zipWith (++) ("" : repeat (' ':b))

debugPrint :: Show a => Trie a -> String
debugPrint = debugPrint' "" where 
    debugPrint' b (Trie m e) = tryAdd e b . fmap (minit . f) $ (M.assocs m) where
      f (h,t) = str ++ debugPrint' (pad ++ b) t where
        str = show h
        pad = ' ' : zipWith const (repeat ' ') str
            
count :: Trie a -> Int
count (Trie m e) = M.foldr ((+) . count) (if e then 1 else 0) m

instance Ord a => Ord (Trie a) where
  compare (Trie a _) (Trie b _) = case compare x y of LT -> LT
                                                      EQ -> compare w z
                                                      GT -> GT
                                                      where (x,w) = M.findMax a
                                                            (y,z) = M.findMax b 
                                                            
alter :: (Ord a, Foldable f) => (Trie a -> Maybe (Trie a)) 
                             -> ((Trie a -> Maybe (Trie a)) 
                             -> Maybe (Trie a) -> Maybe (Trie a)) 
                             -> f a -> Trie a  -> Trie a
alter i o xs = (fromMaybe empty) . (foldr f i xs) where
  f e a = nilIfEmpty . overMap (M.alter (o a) e)
  
zipUntil :: (Ord a, Foldable f) => b -> (Trie a -> b) -> f a -> Trie a -> b
zipUntil base = foldr f where
  f e a = fromMaybe base . fmap a . M.lookup e . getTrie 
