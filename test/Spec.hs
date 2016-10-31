module Main (main) where

import           Data.TrieSet    (TrieBag, TrieSet)
import qualified Data.TrieSet    as TrieSet

import           Test.DocTest
import           Test.QuickCheck

import           Data.List       (stripPrefix)
import           Data.Maybe      (mapMaybe)

addIsMember :: TrieSet Bool String -> String -> Bool
addIsMember t xs = TrieSet.member xs (TrieSet.add xs t)

setOrderDoesntMatter :: [String] -> Property
setOrderDoesntMatter xs =
  (TrieSet.fromList xs :: TrieSet Bool String) === TrieSet.fromList ys .&&.
  (TrieSet.fromList xs :: TrieBag String) === TrieSet.fromList ys
  where ys = reverse xs

notMemberIsntMember :: [String] -> String -> Property
notMemberIsntMember xs x =
  TrieSet.member x (TrieSet.fromList xs) == elem x xs .&&.
  TrieSet.member x (TrieSet.fromList xs) == length (filter (x==) xs)

fromListIsMember :: [String] -> Bool
fromListIsMember xs = all (`TrieSet.member` t) xs
  where t = TrieSet.fromList xs

deletedIsntMember :: [String] -> Bool
deletedIsntMember xs = let t = TrieSet.fromList xs in all not
  [ TrieSet.member x (TrieSet.delete x t) | x <- xs ]

insertDeleteId :: [String] -> String -> Property
insertDeleteId xs x =
  x `notElem` xs ==>
    let ts = TrieSet.fromList xs :: TrieSet Bool String
    in (TrieSet.delete x . TrieSet.add x) ts === ts

suffixesIsSame :: [String] -> Property
suffixesIsSame xs = conjoin
  [ TrieSet.suffixes x t === TrieSet.fromList (mapMaybe (stripPrefix x) xs)
  | x <- xs
  ] where t = TrieSet.fromList xs :: TrieSet Bool String

main :: IO ()
main = do
  quickCheck addIsMember
  quickCheck setOrderDoesntMatter
  quickCheck notMemberIsntMember
  quickCheck fromListIsMember
  quickCheck deletedIsntMember
  quickCheck insertDeleteId
  quickCheck suffixesIsSame
  doctest [ "-isrc"
          , "src/Data/Trie.hs"
          , "src/Data/TrieSet.hs" ]
