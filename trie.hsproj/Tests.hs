module Tests where

import Trie
import Test.QuickCheck
import qualified Data.Set as S
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Control.Monad (liftM2)
import Prelude hiding (null, any)
import Data.Foldable (any)

instance (Arbitrary a , Ord a) => Arbitrary (Trie a) where
  arbitrary = fmap (fromList) (arbitrary :: (Arbitrary a, Ord a) => Gen [[a]])
  
equivFunc :: (Ord a)
          => (d -> S.Set [a] -> b)
          -> (d -> Trie a -> c)
          -> (b -> c -> Bool)
          -> d -> [[a]] -> Bool
equivFunc funcS funcT eq input into = funcS input (S.fromList into)
                                      `eq` 
                                      funcT input (fromList into)
                                      
equivProp :: (Ord a, Eq b)
          => (S.Set [a] -> b)
          -> (Trie a    -> b)
          -> [[a]] -> Bool
equivProp funcS funcT = liftM2 (==) (funcT . fromList) (funcS . S.fromList)

equivMerge :: (Ord a) => (S.Set [a] -> S.Set [a] -> S.Set [a])
                      -> (Trie   a  -> Trie   a  -> Trie   a )
                      -> [[a]]      -> [[a]]     -> Bool
equivMerge sm tm la lb = (S.fromList la `sm` S.fromList lb) 
                         `eqSetTrie` 
                         (fromList la `tm` fromList lb)

eqSetTrie :: Eq a => S.Set [a] -> Trie a -> Bool
eqSetTrie s t = S.toList s == toList t
   
spref :: Ord a => [a] -> S.Set [a] -> S.Set [a]
spref l = S.map (drop (length l)) . S.filter (isPrefixOf l)

stoggle :: Ord a => a -> S.Set a -> S.Set a
stoggle e s = if S.member e s then S.delete e s else S.insert e s

main = do
  quickCheck (equivProp S.null null                              :: [String] ->  Bool           )
  quickCheck (equivProp S.size size                              :: [String] ->  Bool           )
  quickCheck (equivFunc S.member member (==)                     ::  String  -> [String] -> Bool)
  quickCheck (equivFunc spref complete eqSetTrie                 ::  String  -> [String] -> Bool)
  quickCheck (equivFunc (any . isPrefixOf) hasPref (==)          ::  String  -> [String] -> Bool)
  quickCheck (equivFunc (any . isSuffixOf) hasSuff (==)          ::  String  -> [String] -> Bool)
  quickCheck (equivFunc (any . isInfixOf) hasSub (==)            ::  String  -> [String] -> Bool)
  quickCheck (equivFunc S.insert insert eqSetTrie                ::  String  -> [String] -> Bool)
  quickCheck (equivFunc stoggle toggle eqSetTrie                 ::  String  -> [String] -> Bool)
  quickCheck (equivProp S.toList toList                          :: [String] ->  Bool           )
  quickCheck (equivFunc (S.filter . isPrefixOf) begins eqSetTrie ::  String  -> [String] -> Bool)
  quickCheck (equivFunc (S.filter . isSuffixOf) ends   eqSetTrie ::  String  -> [String] -> Bool)
  quickCheck (liftM2 eqSetTrie S.singleton singleton             ::  String  ->  Bool           )
  quickCheck (equivMerge S.union union                           :: [String] -> [String] -> Bool)
  quickCheck (equivMerge S.difference difference                 :: [String] -> [String] -> Bool)
  quickCheck (equivMerge S.intersection intersection             :: [String] -> [String] -> Bool) 
  
--  , symmetricDifference
--  , foldrTrie