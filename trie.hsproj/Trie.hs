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

foldrTrie :: ([a] -> b -> b) -> b -> Trie a -> b
foldrTrie f i (Trie m a) = M.foldrWithKey ff s m where
  s    = if a then f [] i else i
  ff k = flip (foldrTrie $ f . (k :))
    
toList :: Trie a -> [[a]]
toList = foldrTrie (:) []

foldrTrieGen :: (Applicative f, Foldable f, Monoid (f a)) 
             => (f a -> b -> b) 
             -> b -> Trie a -> b
foldrTrieGen f i (Trie m a) = M.foldrWithKey ff s m where
  s    = if a then f mempty i else i
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
  f e a   = (fmap (flip Trie False . M.singleton e) . a . M.lookup e . getTrie =<<)
            
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

overMap :: Ord b 
        => (M.Map a (Trie a) 
        -> M.Map b (Trie b)) 
        -> Trie a -> Trie b
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
                                                            
alter :: (Ord a, Foldable f) 
      => (Trie a -> Maybe (Trie a)) 
      -> ((Trie a -> Maybe (Trie a)) 
      -> Maybe (Trie a) -> Maybe (Trie a)) 
      -> f a -> Trie a  -> Trie a
alter i o xs = (fromMaybe empty) . (foldr f i xs) where
  f e a = nilIfEmpty . overMap (M.alter (o a) e)
  
zipUntil :: (Ord a, Foldable f) => b -> (Trie a -> b) -> f a -> Trie a -> b
zipUntil base = foldr f where
  f e a = fromMaybe base . fmap a . M.lookup e . getTrie 
