module Day.Day2 where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.List (inits, tails)
import Util (windows)

newtype Report = Report [Int]

parseLineAsReport :: Text -> Report
parseLineAsReport = Report . map (read . Text.unpack) . Text.words

parseInput :: Text -> [Report]
parseInput = map parseLineAsReport . Text.lines

checkReportWith :: (Int -> Int -> Bool) -> Int -> Int -> Report -> Bool
checkReportWith bin low high (Report xs) = all go . windows 2 $ xs
 where
  go [one, two] = two `bin` one && abs (two - one) >= low && abs (two - one) <= high
  go _ = error "This should never happen"

checkSteadilyIncreasing, checkSteadilyDecreasing :: Report -> Bool
checkSteadilyIncreasing = checkReportWith (>) 1 3
checkSteadilyDecreasing = checkReportWith (<) 1 3

checkSafe :: Report -> Bool
checkSafe report = checkSteadilyIncreasing report || checkSteadilyDecreasing report

part1 :: Text -> IO Int
part1 = pure . length . filter checkSafe . parseInput

checkReportWithDeletions :: Report -> Bool
checkReportWithDeletions (Report xs) = any checkSafe deletions
 where
  deletions = map Report $ zipWith (++) (inits xs) (map (drop 1) (tails xs))

part2 :: Text -> IO Int
part2 = pure . length . filter checkReportWithDeletions . parseInput
