{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import           Data.TrieSet    (Trie)
import qualified Data.TrieSet    as TrieSet

import           Test.DocTest
import           Test.QuickCheck

import           Data.List       (stripPrefix)
import           Data.Maybe      (mapMaybe)

import           Text.Read


instance (Arbitrary a, Ord a) => Arbitrary (Trie [a]) where
  arbitrary = fmap TrieSet.fromList (arbitrary :: Arbitrary a => Gen [[a]])

addIsMember :: Trie String -> String -> Bool
addIsMember t xs = TrieSet.member xs (TrieSet.insert xs t)

setOrderDoesntMatter :: [String] -> Property
setOrderDoesntMatter xs =
  (TrieSet.fromList xs :: Trie String) === TrieSet.fromList ys
  where ys = reverse xs

notMemberIsntMember :: [String] -> String -> Bool
notMemberIsntMember xs x =
  TrieSet.member x (TrieSet.fromList xs) == elem x xs -- .&&.
  -- TrieSet.member x (TrieSet.fromList xs) == length (filter (x==) xs)

fromListIsMember :: [String] -> Bool
fromListIsMember xs = all (`TrieSet.member` t) xs
  where t = TrieSet.fromList xs

deletedIsntMember :: [String] -> Bool
deletedIsntMember xs = let t = TrieSet.fromList xs in all not
  [ TrieSet.member x (TrieSet.delete x t) | x <- xs ]

insertDeleteId :: [String] -> String -> Property
insertDeleteId xs x =
  x `notElem` xs ==>
    let ts = TrieSet.fromList xs :: Trie String
    in (TrieSet.delete x . TrieSet.insert x) ts === ts

suffixesIsSame :: [String] -> Property
suffixesIsSame xs = conjoin
  [ TrieSet.suffixes x t === TrieSet.fromList (mapMaybe (stripPrefix x) xs)
  | x <- xs
  ] where t = TrieSet.fromList xs :: Trie String

readWorks :: Trie String -> Property
readWorks xs = Right xs === (readEither . show) xs

main :: IO ()
main = do
  quickCheck addIsMember
  quickCheck setOrderDoesntMatter
  quickCheck notMemberIsntMember
  quickCheck fromListIsMember
  quickCheck deletedIsntMember
  quickCheck insertDeleteId
  quickCheck suffixesIsSame
  quickCheck readWorks
  doctest [ "-isrc"
          , "src/" ]
