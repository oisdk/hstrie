module Main (main) where

import Criterion.Main
import Data.Maybe
import qualified Data.TrieSet as TS
import qualified Data.TrieMap as TM

uniqStrings :: [String]
uniqStrings = flip (:) <$> [] : uniqStrings <*> ['a'..'z']

strings :: [String]
strings = take 10000 uniqStrings

assocs :: [(String,Int)]
assocs = zip (strings ++ strings) [1..]

checkAll :: [String] -> Int
checkAll ss = length (filter (`TS.member` t) ss) where
  t = TS.fromList ss

lookupAll :: [(String,Int)] -> Int
lookupAll ss = sum (mapMaybe (`TM.lookup` t) strings) where
  t = TM.fromList ss

main :: IO ()
main = defaultMain
  [ bgroup "fromList" [ bench "1" $ whnf (TS.member "qtn" . TS.fromList) strings
                      , bench "2" $ whnf (TS.member "qto" . TS.fromList) strings ]
  , bgroup "member"   [ bench "1" $ whnf checkAll strings ]
  , bgroup "lookup"   [ bench "1" $ whnf lookupAll assocs ] ]

