module Main (main) where

import Criterion.Main
import qualified Data.TrieSet as TS
import qualified Data.Set as S

uniqStrings :: [String]
uniqStrings = flip (:) <$> [] : uniqStrings <*> ['a'..'z']

n :: Int
n = 10000

inStrings :: [String]
inStrings = take n uniqStrings

outStrings :: [String]
outStrings = take n (drop n uniqStrings)

checkAll :: [String] -> Int
checkAll ss = length (filter (`TS.member` t) (outStrings ++ ss)) where
  t = TS.fromList ss

checkAllS :: [String] -> Int
checkAllS ss = length (filter (`S.member` t) (outStrings ++ ss)) where
  t = S.fromList ss

main :: IO ()
main = defaultMain
  [ bgroup "fromList" [ bench "TrieSet" $ whnf (TS.member "qto" . TS.fromList :: [String] -> Bool) inStrings
                      , bench "Set"     $ whnf (S.member  "qto" . S.fromList ) inStrings ]
  , bgroup "member"   [ bench "TrieSet" $ whnf checkAll  inStrings
                      , bench "Set"     $ whnf checkAllS inStrings ] ]
