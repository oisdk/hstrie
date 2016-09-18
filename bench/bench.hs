module Main (main) where

import Criterion.Main
import Data.Maybe
import qualified Data.TrieSet as TS
import qualified Data.TrieBag as TB
import qualified Data.TrieMap as TM
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Monoid

uniqStrings :: [String]
uniqStrings = flip (:) <$> [] : uniqStrings <*> ['a'..'z']

n :: Int
n = 10000

inStrings :: [String]
inStrings = take n uniqStrings

repStrings :: [String]
repStrings = (take n . concat . repeat . take 10) uniqStrings

outStrings :: [String]
outStrings = take n (drop n uniqStrings)

assocs :: [(String,Int)]
assocs = zip (inStrings ++ inStrings) [1..]

checkAll :: [String] -> Int
checkAll ss = length (filter (`TS.member` t) (outStrings ++ ss)) where
  t = TS.fromList ss

checkAllS :: [String] -> Int
checkAllS ss = length (filter (`S.member` t) (outStrings ++ ss)) where
  t = S.fromList ss

lookupAll :: [(String,Int)] -> Int
lookupAll ss = sum (mapMaybe (`TM.lookup` t) (inStrings ++ outStrings)) where
  t = TM.fromList ss

lookupAllM :: [(String,Int)] -> Int
lookupAllM ss = sum (mapMaybe (`M.lookup` t) (inStrings ++ outStrings)) where
  t = M.fromList ss

main :: IO ()
main = defaultMain
  [ bgroup "TrieBag"  [ bench "foldr"   $ whnf (length . foldr (:) [] . TB.fromList) repStrings
                      , bench "foldMap" $ whnf (getSum . foldMap (const (Sum (1::Int))) . TB.fromList) repStrings]
  , bgroup "fromList" [ bench "TrieSet" $ whnf (TS.member "qto" . TS.fromList) inStrings
                      , bench "Set"     $ whnf (S.member  "qto" . S.fromList ) inStrings ]
  , bgroup "member"   [ bench "TrieSet" $ whnf checkAll  inStrings
                      , bench "Set"     $ whnf checkAllS inStrings ]
  , bgroup "lookup"   [ bench "TrieMap" $ whnf lookupAll  assocs
                      , bench "Map"     $ whnf lookupAllM assocs ] ]
