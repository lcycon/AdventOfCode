module Day3 (part1, part2) where

import Common (Impl, parseLines)
import Common.List (transposeByZipping)
import Control.Arrow (Arrow ((&&&)))
import Control.Lens (Ixed (ix), filtered, (^..), (^?!))
import Data.Functor (($>))
import Data.List (foldl')
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.String (Parser)

newtype ReportLine = ReportLine {entries :: [Int]}
  deriving (Eq, Show)

reportLineP :: Parser ReportLine
reportLineP = ReportLine <$> many1 (choice [char '0' $> 0, char '1' $> 1])

part1 :: Impl
part1 = show . uncurry (*) . (readGammaRate &&& readEpsilonRate) . parseLines reportLineP
  where
    readGammaRate :: [ReportLine] -> Int
    readGammaRate = binaryToInt . fmap mostCommon . transposeByZipping . map entries
    readEpsilonRate :: [ReportLine] -> Int
    readEpsilonRate = binaryToInt . fmap leastCommon . transposeByZipping . map entries

part2 :: Impl
part2 = show . uncurry (*) . (readOxygenRating &&& readScrubberRating) . parseLines reportLineP
  where
    readOxygenRating :: [ReportLine] -> Int
    readOxygenRating = binaryToInt . selectAndFilter mostCommon . map entries
    readScrubberRating :: [ReportLine] -> Int
    readScrubberRating = binaryToInt . selectAndFilter leastCommon . map entries
    selectAndFilter :: ([Int] -> Int) -> [[Int]] -> [Int]
    selectAndFilter selector xs = go 0 xs
      where
        go :: Int -> [[Int]] -> [Int]
        go _ [res] = res
        go idx ys = go (idx + 1) results
          where
            digits = ys ^.. traverse . ix idx
            selected = selector digits
            results = ys ^.. traverse . filtered ((== selected) . (^?! ix idx))

mostCommon :: [Int] -> Int
mostCommon xs = case compare countZeros countOnes of
  LT -> 1
  GT -> 0
  EQ -> 1
  where
    countZeros = length . filter (== 0) $ xs
    countOnes = length . filter (== 1) $ xs

leastCommon :: [Int] -> Int
leastCommon xs = case compare countZeros countOnes of
  LT -> 0
  GT -> 1
  EQ -> 0
  where
    countZeros = length . filter (== 0) $ xs
    countOnes = length . filter (== 1) $ xs

binaryToInt :: [Int] -> Int
binaryToInt = foldl' go 0 . zip [0 ..] . reverse
  where
    go :: Int -> (Int, Int) -> Int
    go acc (power, value) = acc + (value * (2 ^ power))
