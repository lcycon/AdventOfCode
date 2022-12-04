module Day1 (part1, part2) where

import Common (Impl, asInt, readLines)
import Common.List (sliding)

part1 :: Impl
part1 = show . length . filter go . sliding 2 1 . readLines asInt
  where
    {-@ go :: Ord a => {v: [a] | len v = 2} -> Bool @-}
    go [first, second] = first < second

part2 :: Impl
part2 = show . length . filter go . sliding 2 1 . fmap sum . sliding 3 1 . readLines asInt
  where
    {-@ go :: Ord a => {v: [a] | len v = 2} -> Bool @-}
    go [first, second] = first < second
