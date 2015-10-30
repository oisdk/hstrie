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
  , toListDesc) where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Foldable hiding (toList)
import Prelude hiding (foldr, null, all, any)
import Control.Applicative hiding (empty)
import Control.Monad
import Data.Monoid

data Trie a = Trie { getTrie :: M.Map a (Trie a)
                   , endHere :: Bool } deriving (Eq)

empty :: Trie a
empty = Trie M.empty False

null :: Trie a -> Bool
null (Trie m e) = not e && M.null m

insert :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
insert = foldr f (overEnd $ const True) where
  f e a = overMap (M.alter (Just . a . fromMaybe empty) e)

instance Show a => Show (Trie a) where
  show = show . toList

fromList :: (Ord a, Foldable f, Foldable g) => f (g a) -> Trie a
fromList = foldr insert empty

makeList :: ([[a]] -> [[a]] -> [[a]]) -> Trie a -> [[a]]
makeList com (Trie m a) = M.foldlWithKey f (if a then [[]] else []) m where
  f c k = (com c) . map (k :) . makeList com

toList :: Trie a -> [[a]]
toList = makeList (++)

toListDesc :: Trie a -> [[a]]
toListDesc = makeList (flip (++))

foldrTrie :: ([a] -> b -> b) -> b -> Trie a -> b
foldrTrie f i (Trie m a) = M.foldrWithKey ff (if a then f [] i else i) m where
  ff k = flip (foldrTrie $ f . (k :))

foldrT :: (Applicative f, Foldable f, Monoid (f a)) => (f a -> b -> b) -> b -> Trie a -> b
foldrT f i (Trie m a) = M.foldrWithKey ff (if a then f mempty i else i) m where
  ff k = flip (foldrT $ f . mappend (pure k))

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
  
remove :: (Ord a , Foldable f) => f a -> Trie a -> Trie a
remove xs = (fromMaybe empty) . (remove' xs) where
  remove' = foldr f (nilIfEmpty . overEnd (const False))
  f e a   = nilIfEmpty . overMap (M.alter (>>= a) e)
    
hasSub :: (Ord a, Foldable f) => f a -> Trie a -> Bool
hasSub xs t = hasPref xs t || any (hasSub xs) (getTrie t)

overMap :: Ord b => (M.Map a (Trie a) -> M.Map b (Trie b)) -> Trie a -> Trie b
overMap f (Trie m e) = Trie (f m) e

nilIfEmpty :: Trie a -> Maybe (Trie a)
nilIfEmpty t | null t    = Nothing
             | otherwise = Just t

overEnd :: (Bool -> Bool) -> (Trie a -> Trie a)
overEnd f (Trie m e) = Trie m (f e)

zipUntil :: (Ord a, Foldable f) => b -> (Trie a -> b) -> f a -> Trie a -> b
zipUntil base = foldr f where
  f e a = fromMaybe base . fmap a . M.lookup e . getTrie 
           
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
                                                            