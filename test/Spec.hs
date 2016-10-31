module Main (main) where

import           Data.TrieSet    (TrieBag, TrieSet)
import qualified Data.TrieSet    as TrieSet

import           Test.DocTest
import           Test.QuickCheck

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

main :: IO ()
main = do
  quickCheck addIsMember
  quickCheck setOrderDoesntMatter
  quickCheck notMemberIsntMember
  quickCheck fromListIsMember
  quickCheck deletedIsntMember
  quickCheck insertDeleteId
  doctest [ "-isrc"
          , "src/Data/Trie.hs"
          , "src/Data/TrieSet.hs" ]
