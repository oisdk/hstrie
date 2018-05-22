module Main (main) where

import Criterion.Main
import System.Random
import Control.Monad
import qualified Data.Vector as Vector
import Data.Trie.Vector.Set

int :: Int -> IO Int
int n = randomRIO (0,n)

vec :: Int -> IO (Vector.Vector Int)
vec n = randomRIO (0,n) >>= flip Vector.replicateM (int n)

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (replicateM n (vec n)) $
    \xs ->
         bgroup (show n) [bench "fromList" $ nf fromList xs]

main :: IO ()
main = defaultMain (map benchAtSize [1000])
