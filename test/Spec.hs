{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Data.Foldable
import qualified Data.Set             as Set
import qualified Data.Trie.List.Set   as ListTrie
import qualified Data.Trie.Vector.Set as VectorTrie
import qualified Data.Vector          as Vector

memberProp :: (Ord a, Show a, Monoid b) => Gen a -> (a -> b -> b) -> (a -> b -> Bool) -> Property
memberProp gen insert member =
    property $
    do xs <- forAll $ Gen.list (Range.linear 0 100) gen
       let trie = foldr insert mempty xs
       for_ xs $
           \x ->
                assert (x `member` trie)
       let inset = Set.fromList xs
       y <- forAll (Gen.filter (`Set.notMember` inset) gen)
       assert (not (y `member` trie))

prop_memberList :: Property
prop_memberList =
    memberProp
        (Gen.list (Range.linear 0 100) Gen.alpha)
        ListTrie.insert
        ListTrie.member

prop_memberVector :: Property
prop_memberVector =
    memberProp
        (Vector.fromList <$> Gen.list (Range.linear 0 100) Gen.alpha)
        VectorTrie.insert
        VectorTrie.member

main :: IO Bool
main = checkParallel $$(discover)
