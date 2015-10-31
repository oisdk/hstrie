module Tests where
  
import qualified Data.List as L
import Control.Monad
import Trie
import Test.QuickCheck
import qualified Data.Set as S

instance (Arbitrary a , Ord a) => Arbitrary (Trie a) where
  arbitrary = fmap (fromList) (arbitrary :: (Arbitrary a, Ord a) => Gen [[a]])
       
check :: (Ord a, Eq c) => (b -> [[a]] -> c) -> (b -> Trie a -> c) -> b -> [[a]] -> Bool
check funcL func input into = (funcL input into) == (func input (fromList into))

containsL :: Eq a => [a] -> [[a]] -> Bool
containsL = any . (==)

completeL :: (Ord a) => [a] -> [[a]] -> Trie a
completeL l = fromList . map (drop (length l)) . filter (L.isPrefixOf l)

completeT :: (Ord a) => [a] -> [[a]] -> [[a]]
completeT l = map (drop (length l)) . filter (L.isPrefixOf l)
    
beginsL :: Ord a => [a] -> [[a]] -> Trie a
beginsL l = fromList . filter (L.isPrefixOf l)

hasSubL :: Eq a => [a] -> [[a]] -> Bool
hasSubL x l = any (L.isPrefixOf x) ([]:(l >>= L.tails))

hasPrefL :: Eq a => [a] -> [[a]] -> Bool
hasPrefL xs = (any $ L.isPrefixOf xs) . ([]:)

hasSuffL :: Eq a => [a] -> [[a]] -> Bool
hasSuffL = any . L.isSuffixOf

removeL :: Ord a => [a] -> [[a]] -> Trie a
removeL x = fromList . filter (/=x)

xorL :: Ord a => [a] -> [[a]] -> Trie a
xorL x l = if containsL x l then removeL x l else fromList (x : l)

xorL' :: Ord a => [a] -> [[a]] -> [[a]]
xorL' x l = if containsL x l then toList (removeL x l) else x : l

setOp :: Ord a => (S.Set [a] -> S.Set [a] -> S.Set [a]) -> Trie a -> [[a]] -> Trie a
setOp f a b = fromList $ f ((S.fromList . toList) a) (S.fromList b)

sSuffixOf :: Eq a => [a] -> [a] -> Bool
sSuffixOf [] [] = True
sSuffixOf [] _  = False
sSuffixOf x  y  = L.isSuffixOf x y

endsL :: Ord a => [a] -> [[a]] -> Trie a
endsL l  = fromList . filter (sSuffixOf l)

symDiffL :: Ord a => Trie a -> [[a]] -> Trie a
symDiffL t l = fromList (foldr xorL' l (toList t))