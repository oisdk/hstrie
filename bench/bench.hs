module Main (main) where

import Criterion.Main
import System.Random
import Control.Monad
import Lib

int :: Int -> IO Int
int n = randomRIO (0,n)

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (replicateM n (int n)) $
    \xs ->
         bgroup (show n) [bench "someFunc" $ nf (map (const someFunc)) xs]

main :: IO ()
main = defaultMain (map benchAtSize [100])
