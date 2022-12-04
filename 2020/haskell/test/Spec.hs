{-# LANGUAGE OverloadedStrings #-}

import Advent (Part (Part1, Part2))
import Data.Text (Text)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import Lib (debugAdvent)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

checkPart :: Integer -> Part -> (Text -> String) -> String -> SpecWith ()
checkPart day part func answer = describe ("Day" ++ show day) $ do
  it ("passes " ++ show part) $ do
    result <- debugAdvent day part func
    result `shouldBe` answer

main :: IO ()
main = hspec $ do
  checkPart 1 Part1 Day1.part1 "539851"
  checkPart 1 Part2 Day1.part2 "212481360"

  checkPart 2 Part1 Day2.part1 "546"
  checkPart 2 Part2 Day2.part2 "275"

  checkPart 3 Part1 Day3.part1 "211"
  checkPart 3 Part2 Day3.part2 "3584591857"

  checkPart 4 Part1 Day4.part1 "254"
  checkPart 4 Part2 Day4.part2 "184"

  checkPart 5 Part1 Day5.part1 "892"
  checkPart 5 Part2 Day5.part2 "625"
