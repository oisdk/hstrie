{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad
import           Data.Functor
import qualified Data.Set        as Set
import qualified Data.Map as Map
-- import qualified Data.Trie       as Trie
import qualified Data.TrieBag    as TrieBag
-- import qualified Data.TrieBin    as TrieBin
import qualified Data.TrieMap    as TrieMap
import qualified Data.TrieSet    as TrieSet
import           System.Exit
import           Test.DocTest
import           Test.QuickCheck
import Data.List

prop_sameSize :: [String] -> Property
prop_sameSize xs =
  length (Set.fromList xs) === length (TrieSet.fromList xs) .&&.
  length xs === length (TrieBag.fromList xs)

prop_sameCont :: [(String,Int)] -> Property
prop_sameCont xs =
  (sort . Map.toList . Map.fromList . reverse) xs === (sort . TrieMap.assocs . TrieMap.fromList) xs

prop_orderedList :: [String] -> Property
prop_orderedList xs =
  (foldr (:) [] . TrieSet.fromList) xs === (Set.toList . Set.fromList) xs

prop_orderedListBag :: [String] -> Property
prop_orderedListBag xs =
  (foldr (:) [] . TrieBag.fromList) xs === sort xs

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

return []

runTests :: IO Bool
runTests = $forAllProperties quickCheckExit

main :: IO Bool
main = do
  doctest [ "-isrc"
          , "src/Data/Trie.hs"
          , "src/Data/TrieBin.hs"
          , "src/Data/TrieBag.hs"
          , "src/Data/TrieMap.hs"
          , "src/Data/TrieSet.hs" ]
  runTests
