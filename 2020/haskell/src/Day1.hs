module Day1 (part1, part2) where

import Common (Impl, asInt, readLines)

part1 :: Impl
part1 input = show . head $ [a * b | a <- expenseReport, b <- expenseReport, a + b == 2020]
  where
    expenseReport = readLines asInt input

part2 :: Impl
part2 input = show . head $ [a * b * c | a <- expenseReport, b <- expenseReport, c <- expenseReport, a + b + c == 2020]
  where
    expenseReport = readLines asInt input
