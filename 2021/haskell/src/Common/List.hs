module Common.List where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.List (tails)

transposeByZipping :: [[a]] -> [[a]]
transposeByZipping = getZipList . traverse ZipList

every :: Int -> [a] -> [a]
every n xs = case xs of
  y : ys -> y : every n (drop (n - 1) ys)
  [] -> []

sliding :: Int -> Int -> [a] -> [[a]]
sliding window step = transposeByZipping . take window . fmap (every step) . tails
