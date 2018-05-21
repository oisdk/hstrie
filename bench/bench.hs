module Main (main) where

import Criterion.Main
import System.Random
import Control.Monad
import Data.List

int :: Int -> IO Int
int n = randomRIO (0,n)

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (replicateM n (int n)) $
    \xs ->
         bgroup (show n) [bench "someFunc" $ nf sort xs]

main :: IO ()
main = defaultMain (map benchAtSize [100])
