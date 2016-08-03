{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Function
import           Data.Functor
import qualified Data.Set                 as Set
import           Data.Trie
import           GHC.Exts
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Test.DocTest

prop_checkFromList :: [String] -> Bool
prop_checkFromList = sameResult (tfl.toList.tfl) tfl

tfl :: [String] -> TrieSet String
tfl = fromList

prop_checkWithSet :: [String] -> Bool
prop_checkWithSet = sameResult (Set.fromList . foldr (:) [] . tfl) Set.fromList

prop_checkUnion :: [String] -> [String] -> Bool
prop_checkUnion = sameResult2 (mappend `on` tfl) (\x y -> (tfl . Set.toList) $ (mappend `on` Set.fromList) x y)

sameResult :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
sameResult = liftA2 (==)

isId :: Eq a => (a -> a) -> a -> Bool
isId = sameResult id

sameResult2 :: Eq c => (a -> b -> c) -> (a -> b -> c) -> a -> b -> Bool
sameResult2 = liftA2 sameResult

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

failWith :: String -> P.Result
failWith r = P.failed { P.reason = r }

return []

runTests :: IO Bool
runTests = $forAllProperties quickCheckExit

main :: IO Bool
main = do
  doctest ["-isrc", "src/Data/Trie.hs"]
  runTests
