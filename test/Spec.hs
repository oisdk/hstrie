{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Control.Monad
import           Data.Foldable
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector

import qualified Data.Trie.List.Set   as ListTrie
import qualified Data.Trie.Vector.Set as VectorTrie

type ListTrie = ListTrie.Trie
type VectorTrie = VectorTrie.Trie

data Mapping a b = Mapping
    { insert :: a -> b -> b
    , empty  :: b
    , member :: a -> b -> Bool
    , delete :: a -> b -> b
    , gen    :: Gen a
    }

listTrie
    :: Mapping String (ListTrie String)
listTrie =
    Mapping
        ListTrie.insert
        (ListTrie.Trie False Map.empty)
        ListTrie.member
        ListTrie.delete
        (Gen.list (Range.linear 0 100) Gen.alpha)

vectorTrie
    :: Mapping (Vector Char) (VectorTrie String)
vectorTrie =
    Mapping
        VectorTrie.insert
        (VectorTrie.Trie Vector.empty False Map.empty)
        VectorTrie.member
        VectorTrie.delete
        (Vector.fromList <$> Gen.list (Range.linear 0 100) Gen.alpha)

memberProp :: (Ord a, Show a) => Mapping a b -> Property
memberProp Mapping{..} =
    property $
    do xs <- forAll $ Gen.list (Range.linear 0 100) gen
       let trie = foldr insert empty xs
       for_ xs $
           \x ->
                assert (x `member` trie)
       let inset = Set.fromList xs
       y <- forAll (Gen.filter (`Set.notMember` inset) gen)
       assert (not (y `member` trie))

foldableProp :: (Show a, Foldable f, Show (f a), Ord a) => Gen (f a) -> Property
foldableProp gen = property $ do
    xs <- forAll gen
    let ys = toList xs
    length ys === length xs
    foldr (:) [] xs === ys
    foldl (flip (:)) [] xs === reverse ys
    null xs === null ys
    for_ ys $ \y -> assert (y `elem` xs)
    foldMap (:[]) xs === ys
    foldr' (:) [] xs === ys
    foldl' (flip (:)) [] xs === reverse ys
    unless (null ys) $ do
        minimum xs === minimum ys
        maximum xs === maximum ys

deleteProp :: (Ord a, Show a, Eq b, Show b) => Mapping a b -> Property
deleteProp Mapping{..} = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) gen
    let trie = foldr insert empty xs
    let inset = Set.fromList xs
    y <- forAll (Gen.filter (`Set.notMember` inset) gen)
    let deld = (delete y . insert y) trie
    assert (not (member y deld))
    trie === deld

prop_memberList :: Property
prop_memberList = memberProp listTrie

prop_deleteList :: Property
prop_deleteList = deleteProp listTrie

prop_foldableList :: Property
prop_foldableList = foldableProp (gen listTrie)

prop_memberVector :: Property
prop_memberVector = memberProp vectorTrie

prop_deleteVector :: Property
prop_deleteVector = deleteProp vectorTrie

prop_foldableVector :: Property
prop_foldableVector = foldableProp (gen vectorTrie)

main :: IO Bool
main = checkParallel $$(discover)
