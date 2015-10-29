import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Foldable hiding (toList)
import Prelude hiding (foldr, null)
import Control.Applicative hiding (empty)
import Control.Monad

data Trie a = Trie { getTrie :: M.Map a (Trie a), endHere :: Bool } deriving (Eq)

instance Show a => Show (Trie a) where
  show  = show . toList

overMap :: Ord b => (M.Map a (Trie a) -> M.Map b (Trie b)) -> Trie a -> Trie b
overMap f (Trie m e) = Trie (f m) e

zipUntil :: (Ord a, Foldable f) => b -> (Trie a -> b) -> f a -> Trie a -> b
zipUntil base = foldr f where
  f e a = fromMaybe base . fmap a . M.lookup e . getTrie 

null :: Trie a -> Bool
null (Trie m e) = M.null m

empty :: Trie a
empty = Trie M.empty False

toTrue :: Trie a -> Trie a
toTrue (Trie m _) = Trie m True

toMaybe :: Trie a -> Maybe (Trie a)
toMaybe t | null t    = Nothing
          | otherwise = Just t

insert :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
insert = foldr f toTrue where
  f e a = overMap (M.alter (Just . a . fromMaybe empty) e)
  
woSubs :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
woSubs = getSubs (const Nothing) Just
           
wiSubs :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
wiSubs = getSubs Just toMaybe
           
getSubs :: (Ord a, Foldable f) => (Trie a -> Maybe (Trie a)) -> (Trie a -> Maybe (Trie a)) -> f a -> Trie a -> Trie a
getSubs b g xs = fromMaybe empty . getSubs' xs where
  getSubs' = foldr f b where
  f e a = g . overMap (M.mapMaybeWithKey ff) where
    ff k v | k == e    = a v
           | otherwise = getSubs' xs v
           
contains :: (Ord a, Foldable f) => f a -> Trie a -> Bool
contains = zipUntil False endHere

complete :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
complete = zipUntil empty id

begins :: (Ord a, Foldable f) => f a -> Trie a -> Trie a
begins = foldr f id where
      f e a (Trie m n) = fromMaybe empty (flip Trie n   . 
                                          M.singleton e . 
                                          a <$> M.lookup e m)

fromList :: (Ord a, Foldable f, Foldable g) => f (g a) -> Trie a
fromList = foldr insert empty

toList :: Trie a -> [[a]]
toList (Trie m a) | a         = [] : rest
                  | otherwise = rest where
                  rest = M.assocs m >>= uncurry (fmap . (:)) . fmap toList
                  
count :: Trie a -> Integer
count (Trie m e) = M.foldr ((+) . count) (if e then 1 else 0) m

instance Ord a => Ord (Trie a) where
  compare (Trie a _) (Trie b _) = case compare x y of LT -> LT
                                                      EQ -> compare w z
                                                      GT -> GT
                                                      where (x,w) = M.findMin a
                                                            (y,z) = M.findMin b
                                                            
