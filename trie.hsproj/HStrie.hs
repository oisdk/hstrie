import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Foldable hiding (toList)
import Prelude hiding (foldr, null, all, any)
import Control.Applicative hiding (empty)
import qualified Data.List as L
import Control.Monad

data Trie a = Trie { getTrie :: M.Map a (Trie a), endHere :: Bool } deriving (Eq)

empty :: Trie a
empty = Trie M.empty False

insert :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
insert = foldr f toTrue where
  f e a = overMap (M.alter (Just . a . fromMaybe empty) e)

instance Show a => Show (Trie a) where
  show  = show . toList

fromList :: (Ord a, Foldable f, Foldable g) => f (g a) -> Trie a
fromList = foldr insert empty

toList :: Trie a -> [[a]]
toList (Trie m a) | a         = [] : rest
                  | otherwise = rest where
                  rest = M.assocs m >>= uncurry (fmap . (:)) . fmap toList

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
  
hasSub :: (Ord a, Foldable f) => f a -> Trie a -> Bool
hasSub xs t = hasPref xs t || any (hasSub xs) (getTrie t)

overMap :: Ord b => (M.Map a (Trie a) -> M.Map b (Trie b)) -> Trie a -> Trie b
overMap f (Trie m e) = Trie (f m) e

zipUntil :: (Ord a, Foldable f) => b -> (Trie a -> b) -> f a -> Trie a -> b
zipUntil base = foldr f where
  f e a = fromMaybe base . fmap a . M.lookup e . getTrie 

toTrue :: Trie a -> Trie a
toTrue (Trie m _) = Trie m True
           
minit :: String -> String
minit [] = []
minit ('\n':[]) = []
minit (x:xs) = x : minit xs

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

containsL :: Eq a => [a] -> [[a]] -> Bool
containsL = any . (==)

completeL :: (Ord a) => [a] -> [[a]] -> Trie a
completeL l = fromList . map (drop (length l)) . filter (L.isPrefixOf l)

check :: (Ord a, Eq c) => ([a] -> Trie a -> c) -> ([a] -> [[a]] -> c) -> [[a]] -> Bool
check tf lf l = all (liftM2 (==) tfa lfa) (liftM2 (++) L.tails L.inits =<< l)  where
  t   = fromList l
  tfa = flip tf t
  lfa = flip lf l
      
beginsL :: Ord a => [a] -> [[a]] -> Trie a
beginsL l = fromList . filter (L.isPrefixOf l)

hasSubL :: Eq a => [a] -> [[a]] -> Bool
hasSubL x l = any (L.isPrefixOf x) (l >>= L.tails)

hasPrefL :: Eq a => [a] -> [[a]] -> Bool
hasPrefL = any . L.isPrefixOf

hasSuffL :: Eq a => [a] -> [[a]] -> Bool
hasSuffL = any . L.isSuffixOf
