module Util where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Char (isDigit)
import Data.List (tails)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows count = transpose' . take count . tails

digitBoundedIntP :: Int -> Int -> ReadP Int
digitBoundedIntP a b = read <$> R.choice (map (`R.count` R.satisfy isDigit) [a .. b])
