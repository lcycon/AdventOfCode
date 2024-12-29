module Day.Day1 where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.List (sort)
import qualified Data.Map as M
import Data.Text (Text, lines, unpack, words)

eatLine :: Text -> (Int, Int)
eatLine input = case Data.Text.words input of
  [one, two] -> (asInt one, asInt two)
  _ -> error "Unexpected input"
 where
  asInt = read . unpack

part1 :: Text -> IO Int
part1 input = pure . sum $ zipWith numDifference listOne listTwo
 where
  (listOne, listTwo) = biMapTuple sort . unzip . map eatLine . Data.Text.lines $ input
  numDifference a b = abs (a - b)
  biMapTuple = join (***)

part2 :: Text -> IO Int
part2 input = pure $ foldl go 0 listOne
 where
  (listOne, listTwo) = unzip . map eatLine . Data.Text.lines $ input
  secondListMap = M.fromListWith (+) . flip zip (repeat 1) $ listTwo
  go acc e = acc + (e * M.findWithDefault 0 e secondListMap)
